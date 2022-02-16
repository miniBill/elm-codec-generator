module Editor exposing (getFile)

import Dict exposing (Dict)
import Elm
import Elm.Annotation
import Elm.Case exposing (Branch)
import Elm.Let
import FileParser exposing (typeToString)
import Gen.Array
import Gen.Debug
import Gen.Dict
import Gen.Element as Element
import Gen.Maybe
import Gen.Result
import Gen.Set
import Gen.Theme
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Parser exposing ((|.), Parser)
import Utils exposing (firstLower, firstUpper, typeToDefault)


getFile : List (Result String TypeDecl) -> String
getFile typeDecls =
    let
        filteredDecls =
            List.filter isNotExcluded <|
                List.filterMap Result.toMaybe typeDecls

        declarations =
            let
                decls =
                    filteredDecls
                        |> List.map
                            (\d ->
                                case d of
                                    Alias n _ ->
                                        ( n, d )

                                    Custom n _ ->
                                        ( n, d )
                            )
                        |> Dict.fromList
            in
            List.map (typeDeclToEditor decls) filteredDecls

        defaults =
            filteredDecls
                |> List.map typeDeclToDefault

        errors =
            List.filterMap
                (\r ->
                    case r of
                        Err e ->
                            Just e

                        Ok _ ->
                            Nothing
                )
                typeDecls

        comment =
            if List.isEmpty errors then
                ""

            else
                "\n\n-- " ++ String.join "\n-- " errors

        docs groups =
            List.map
                (\grouped ->
                    "@docs " ++ String.join ", " grouped.members
                )
                groups
    in
    (Elm.fileWith [ "Editors" ]
        { docs = docs
        , aliases =
            [ ( [ "Element", "Input" ], "Input" )
            , ( [ "Element", "Border" ], "Border" )
            , ( [ "Element", "Background" ], "Background" )
            , ( [ "Element", "Font" ], "Font" )
            ]
        }
        (declarations ++ defaults)
    ).contents
        ++ comment


isNotExcluded : TypeDecl -> Bool
isNotExcluded decl =
    let
        excluded =
            [ "A11yOptions"
            , "ChatHistory"
            , "ChatLine"
            , "Data"
            , "GameModel"
            , "Id"
            , "MapModel"
            , "MenuModel"
            , "SharedGameModel"
            , "TalkingModel"
            ]

        _ =
            -- "Clean this up"
            Debug.todo
    in
    not <|
        List.member
            (case decl of
                Alias a _ ->
                    a

                Custom c _ ->
                    c
            )
            excluded


typeDeclToEditor : Dict String TypeDecl -> TypeDecl -> Elm.Declaration
typeDeclToEditor decls decl =
    let
        ( name, view ) =
            case decl of
                Alias n t ->
                    ( n, typeToEditor decls t )

                Custom n vs ->
                    ( n, customEditor decls n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name

        editorName =
            firstLower name ++ "Editor"

        declaration =
            (\value ->
                view value
                    |> Elm.withType
                        (Gen.Theme.types_.editor tipe)
            )
                |> Elm.fn "value"
                |> Elm.declaration editorName
                |> Elm.expose
    in
    declaration


typeDeclToDefault : TypeDecl -> Elm.Declaration
typeDeclToDefault decl =
    let
        ( name, default ) =
            case decl of
                Alias n t ->
                    ( n, typeToDefault t )

                Custom n vs ->
                    ( n, customTypeToDefault n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name
    in
    default
        |> Elm.withType tipe
        |> Elm.declaration (firstLower name ++ "Default")
        |> Elm.expose


customTypeToDefault : String -> List Variant -> Elm.Expression
customTypeToDefault name variants =
    let
        isVariantRecursive ( _, args ) =
            List.any isEmptyBecauseRecursive args

        isEmptyBecauseRecursive tipe =
            case tipe of
                Unit ->
                    False

                Maybe _ ->
                    False

                List _ ->
                    False

                Array _ ->
                    False

                Dict _ _ ->
                    False

                Set _ ->
                    False

                Tuple l r ->
                    isEmptyBecauseRecursive l || isEmptyBecauseRecursive r

                Triple l m r ->
                    isEmptyBecauseRecursive l || isEmptyBecauseRecursive m || isEmptyBecauseRecursive r

                Result _ o ->
                    isEmptyBecauseRecursive o

                Object fs ->
                    List.any (\( _, t ) -> isEmptyBecauseRecursive t) fs

                Named n ->
                    n == name
    in
    variants
        |> List.filter (not << isVariantRecursive)
        |> List.sortBy (\( _, variantArgs ) -> List.length variantArgs)
        |> List.head
        |> Maybe.map
            (\( variantName, variantArgs ) ->
                variantArgs
                    |> List.map typeToDefault
                    |> Elm.apply
                        (Elm.value
                            { importFrom = [ "Model" ]
                            , name = variantName
                            , annotation = Nothing
                            }
                        )
            )
        |> Maybe.withDefault
            ("It is not possible to generate a default for a custom type with no variants"
                |> Elm.string
                |> Gen.Debug.todo
            )


customEditor : Dict String TypeDecl -> String -> List Variant -> Elm.Expression -> Elm.Expression
customEditor decls typeName variants value =
    -- TODO: Deduplicate dis :/
    let
        extractedFields =
            variants
                |> List.foldr
                    (\( _, args ) acc ->
                        args
                            |> List.foldl
                                (\argType variablesCount ->
                                    let
                                        variable =
                                            typeToVariable argType
                                    in
                                    case Dict.get variable variablesCount of
                                        Nothing ->
                                            Dict.insert variable ( 1, argType ) variablesCount

                                        Just ( count, _ ) ->
                                            Dict.insert variable ( count + 1, argType ) variablesCount
                                )
                                Dict.empty
                            |> Dict.union acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.concatMap
                    (\( name, ( n, tipe ) ) ->
                        List.range 1 n
                            |> List.map
                                (\i ->
                                    ( if i == 1 then
                                        name ++ "Extracted"

                                      else
                                        name ++ String.fromInt i ++ "Extracted"
                                    , typeToDefault tipe
                                    )
                                )
                    )

        extractedDefault =
            extractedFields
                |> List.map (\( name, fieldDefault ) -> Elm.field name fieldDefault)
                |> Elm.record

        extractedPattern : List String
        extractedPattern =
            List.map Tuple.first extractedFields

        extractedValues extractedDefault_ =
            variants
                |> List.map
                    (\( variantName, args ) ->
                        let
                            argNames =
                                args
                                    |> List.foldl
                                        (\argType ( variables, variablesCount ) ->
                                            let
                                                variable =
                                                    typeToVariable argType
                                            in
                                            case Dict.get variable variablesCount of
                                                Nothing ->
                                                    ( variable :: variables
                                                    , Dict.insert variable 2 variablesCount
                                                    )

                                                Just count ->
                                                    ( (variable ++ String.fromInt count) :: variables
                                                    , Dict.insert variable (count + 1) variablesCount
                                                    )
                                        )
                                        ( [], Dict.empty )
                                    |> Tuple.first
                                    |> List.reverse
                        in
                        Elm.Case.branchWith [ "Model" ]
                            variantName
                            (List.length args)
                            (\numberedArgs ->
                                if List.isEmpty argNames then
                                    extractedDefault_

                                else
                                    List.map2 (\name numberedArg -> Elm.field (name ++ "Extracted") numberedArg) argNames numberedArgs
                                        |> Elm.updateRecord extractedDefault_
                            )
                    )
                |> Elm.Case.custom value

        inputsRow =
            variants
                |> List.map (variantToInputsRowCase decls typeName)
                |> Elm.Case.custom value

        variantsTuples =
            Elm.list <| List.map variantToTuple variants

        variantToTuple ( variantName, args ) =
            let
                ctorArgs =
                    args
                        |> List.foldl
                            (\argType ( variables, variablesCount ) ->
                                let
                                    variableBasename =
                                        typeToVariable argType

                                    ( variable, newCount ) =
                                        case Dict.get variableBasename variablesCount of
                                            Nothing ->
                                                ( variableBasename ++ "Extracted"
                                                , 2
                                                )

                                            Just count ->
                                                ( variableBasename ++ String.fromInt count ++ "Extracted"
                                                , count + 1
                                                )
                                in
                                ( localValue variable :: variables
                                , Dict.insert variableBasename newCount variablesCount
                                )
                            )
                            ( [], Dict.empty )
                        |> Tuple.first
                        |> List.reverse
            in
            Elm.tuple
                (Elm.string <|
                    splitOnUppercase <|
                        if String.startsWith typeName variantName then
                            String.dropLeft (String.length typeName) variantName

                        else
                            variantName
                )
                (if List.isEmpty ctorArgs then
                    Elm.value { importFrom = [ "Model" ], name = variantName, annotation = Nothing }

                 else
                    Elm.apply (Elm.value { importFrom = [ "Model" ], name = variantName, annotation = Nothing }) ctorArgs
                )
    in
    if List.isEmpty extractedFields then
        Elm.Let.letIn
            (\variants_ ->
                Gen.Theme.enumEditor variants_ value
            )
            |> Elm.Let.value "variants" variantsTuples
            |> Elm.Let.toExpression

    else
        Elm.Let.letIn (\variants_ inputsRow_ _ _ -> Gen.Theme.customEditor variants_ inputsRow_ value)
            |> Elm.Let.value "variants" variantsTuples
            |> Elm.Let.value "inputsRow" inputsRow
            |> Elm.Let.value "extractedDefault" extractedDefault
            |> Elm.Let.record extractedPattern (extractedValues (localValue "extractedDefault"))
            |> Elm.Let.toExpression


splitOnUppercase : String -> String
splitOnUppercase str =
    str
        |> String.toList
        |> List.concatMap
            (\c ->
                if Char.isUpper c then
                    [ ' ', Char.toLower c ]

                else
                    [ c ]
            )
        |> String.fromList
        |> String.trim
        |> firstUpper


typeToVariable : Type -> String
typeToVariable tipe =
    let
        innerTypeToVariable t =
            case t of
                Unit ->
                    "Unit"

                Maybe i ->
                    "Maybe" ++ innerTypeToVariable i

                List i ->
                    "List" ++ innerTypeToVariable i

                Array i ->
                    "Array" ++ innerTypeToVariable i

                Set i ->
                    "Set" ++ innerTypeToVariable i

                Dict l r ->
                    "Dict" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Tuple l r ->
                    "Tuple" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Result l r ->
                    "Result" ++ innerTypeToVariable l ++ innerTypeToVariable r

                Triple l m r ->
                    "Triple" ++ innerTypeToVariable l ++ innerTypeToVariable m ++ innerTypeToVariable r

                Object fields ->
                    fields
                        |> List.map
                            (\( fname, ftipe ) ->
                                fname ++ innerTypeToVariable ftipe
                            )
                        |> String.concat

                Named n ->
                    n
    in
    firstLower <| innerTypeToVariable tipe


variantToInputsRowCase : Dict String TypeDecl -> String -> ( String, List Type ) -> Branch
variantToInputsRowCase decls typeName ( variantName, args ) =
    let
        argNamesAndTypes =
            args
                |> List.foldl
                    (\argType ( variables, variablesCount ) ->
                        let
                            variableBasename =
                                typeToVariable argType

                            ( variableName, newCount ) =
                                case Dict.get variableBasename variablesCount of
                                    Nothing ->
                                        ( variableBasename
                                        , 2
                                        )

                                    Just count ->
                                        ( variableBasename ++ String.fromInt count
                                        , count + 1
                                        )
                        in
                        ( ( variableName, argType ) :: variables
                        , Dict.insert variableBasename newCount variablesCount
                        )
                    )
                    ( [], Dict.empty )
                |> Tuple.first
                |> List.reverse
    in
    Elm.Case.branchWith [ "Model" ]
        variantName
        (List.length args)
        (\indexedArgs ->
            List.map2 Tuple.pair
                argNamesAndTypes
                indexedArgs
                |> List.indexedMap
                    (\i ( ( _, tipe ), arg ) ->
                        Gen.Theme.map
                            (\newValue ->
                                Elm.apply (Elm.value { importFrom = [ "Model" ], name = variantName, annotation = Nothing })
                                    (List.indexedMap
                                        (\j originalArg ->
                                            if i == j then
                                                newValue

                                            else
                                                originalArg
                                        )
                                        indexedArgs
                                    )
                            )
                            (Elm.Annotation.function
                                (List.map (\( _, t ) -> typeToAnnotation t) argNamesAndTypes)
                                (Elm.Annotation.namedWith [ "Model" ] typeName [])
                            )
                            (typeToEditor decls tipe arg)
                    )
                |> Elm.list
        )


typeToEditor : Dict String TypeDecl -> Type -> Elm.Expression -> Elm.Expression
typeToEditor decls =
    typeToEditorAndDefault decls >> Tuple.first


localValue : String -> Elm.Expression
localValue name =
    Elm.value
        { importFrom = []
        , name = name
        , annotation = Nothing
        }


typeToEditorAndDefault : Dict String TypeDecl -> Type -> ( Elm.Expression -> Elm.Expression, Elm.Expression )
typeToEditorAndDefault decls tipe =
    let
        typeToEditorNameAndDefault t =
            let
                ( ed, def ) =
                    typeToEditorAndDefault decls t
            in
            ( Elm.functionReduced "value" (typeToAnnotation tipe) ed
            , def
            )

        map ef df t1 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1
            in
            ( \value ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Frontend", "EditorTheme" ]
                        , name = ef
                        , annotation = Nothing
                        }
                    )
                    [ Elm.string <| typeToString True t1
                    , e1
                    , d1
                    , value
                    ]
            , df d1
            )

        map2 ef df t1 t2 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1

                ( e2, d2 ) =
                    typeToEditorNameAndDefault t2
            in
            ( \value ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Frontend", "EditorTheme" ]
                        , name = ef
                        , annotation = Nothing
                        }
                    )
                    [ e1
                    , d1
                    , e2
                    , d2
                    , value
                    ]
            , df d1 d2
            )

        map3 ef df t1 t2 t3 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1

                ( e2, d2 ) =
                    typeToEditorNameAndDefault t2

                ( e3, d3 ) =
                    typeToEditorNameAndDefault t3
            in
            ( \value ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Frontend", "EditorTheme" ]
                        , name = ef
                        , annotation = Nothing
                        }
                    )
                    [ e1
                    , d1
                    , e2
                    , d2
                    , e3
                    , d3
                    , value
                    ]
            , df d1 d2 d3
            )
    in
    case tipe of
        Unit ->
            ( \_ -> Element.none, Elm.unit )

        Maybe inner ->
            map "maybeEditor" (always Gen.Maybe.types_.maybe.create.nothing) inner

        List inner ->
            map "listEditor" (always (Elm.list [])) inner

        Array inner ->
            map "arrayEditor" (always Gen.Array.empty) inner

        Set inner ->
            map "setEditor" (always Gen.Set.empty) inner

        Dict k v ->
            map2 "dictEditor" (\_ _ -> Gen.Dict.empty) k v

        Tuple a b ->
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault a

                ( e2, d2 ) =
                    typeToEditorNameAndDefault b
            in
            ( Gen.Theme.tupleEditor e1 (isSimple decls a) e2 (isSimple decls b), Elm.tuple d1 d2 )

        Result e o ->
            map2 "resultEditor" (\_ okDefault -> Gen.Result.types_.result.create.ok okDefault) e o

        Triple a b c ->
            map3 "tripleEditor" Elm.triple a b c

        Object fields ->
            objectEditorAndDefault decls tipe fields

        Named n ->
            let
                simple default =
                    ( \value ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Frontend", "EditorTheme" ]
                                , name = firstLower n ++ "Editor"
                                , annotation = Nothing
                                }
                            )
                            [ value ]
                    , default
                    )
            in
            case n of
                "String" ->
                    simple <| Elm.string ""

                "Char" ->
                    simple <| Elm.char ' '

                "Bool" ->
                    simple <| Elm.bool True

                "Int" ->
                    simple <| Elm.int 0

                "Float" ->
                    simple <| Elm.float 0

                _ ->
                    ( \value ->
                        Elm.apply (localValue <| firstLower n ++ "Editor")
                            [ value ]
                    , localValue <| firstLower n ++ "Default"
                    )


isSimple : Dict String TypeDecl -> Type -> Bool
isSimple decls tipe =
    case tipe of
        Unit ->
            True

        Named n ->
            if n == "String" || n == "Char" || n == "Bool" || n == "Int" || n == "Float" then
                True

            else
                case Dict.get n decls of
                    Nothing ->
                        False

                    Just (Custom _ _) ->
                        False

                    Just (Alias _ t) ->
                        isSimple decls t

        _ ->
            False


objectEditorAndDefault :
    Dict String TypeDecl
    -> Type
    -> List ( String, Type )
    ->
        ( Elm.Expression -> Elm.Expression
        , Elm.Expression
        )
objectEditorAndDefault decls tipe fields =
    ( \value ->
        let
            raw =
                fields
                    |> List.map
                        (\( fieldName, fieldType ) ->
                            { fieldName = fieldName
                            , fieldType = fieldType
                            , editor =
                                typeToEditor decls
                                    fieldType
                                    (Elm.get fieldName value)
                            , simple = isSimple decls fieldType
                            }
                        )

            rawSimples =
                List.concatMap
                    (\{ fieldName, fieldType, editor, simple } ->
                        if simple then
                            [ Elm.tuple
                                (Elm.string <| splitOnUppercase fieldName)
                                (Gen.Theme.map
                                    (\newValue ->
                                        updateExpression value [ Elm.field fieldName newValue ]
                                    )
                                    (mapAnnotation fieldType tipe)
                                    (Elm.withType
                                        (Gen.Theme.types_.editor (typeToAnnotation fieldType))
                                        editor
                                    )
                                )
                            ]

                        else
                            []
                    )
                    raw

            rawComplexes =
                List.concatMap
                    (\{ fieldName, fieldType, editor, simple } ->
                        if simple then
                            []

                        else
                            [ Elm.tuple
                                (Elm.string <| splitOnUppercase fieldName)
                                (Gen.Theme.map
                                    (\newValue ->
                                        updateExpression value [ Elm.field fieldName newValue ]
                                    )
                                    (mapAnnotation fieldType tipe)
                                    (Elm.withType
                                        (Gen.Theme.types_.editor (typeToAnnotation fieldType))
                                        editor
                                    )
                                )
                            ]
                    )
                    raw
        in
        Gen.Theme.objectEditor (Elm.list rawSimples) (Elm.list rawComplexes)
    , fields
        |> List.map
            (\( fieldName, fieldType ) ->
                fieldType
                    |> typeToDefault
                    |> Elm.field fieldName
            )
        |> Elm.record
    )


mapAnnotation : Type -> Type -> Elm.Annotation.Annotation
mapAnnotation fieldType valueType =
    Elm.Annotation.function
        [ typeToAnnotation fieldType ]
        (typeToAnnotation valueType)


variableParser : Parser ()
variableParser =
    Parser.succeed ()
        |. Parser.chompIf Char.isLower
        |. Parser.chompWhile (\c -> c == '_' || Char.isAlphaNum c)
        |. Parser.end


updateExpression : Elm.Expression -> List Elm.Field -> Elm.Expression
updateExpression value fields =
    -- let
    --     str =
    --         Elm.toString value
    -- in
    -- case Parser.run variableParser str of
    --     Err _ ->
    --         Elm.letIn
    --             [ Elm.Let.value "updating" value ]
    --             (Elm.updateRecord "updating" fields)
    --     Ok _ ->
    --         Elm.updateRecord str fields
    Elm.updateRecord value fields
