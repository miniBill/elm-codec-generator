module Editor exposing (getFile)

import Dict
import Elm
import Elm.Annotation
import Elm.Gen.Array
import Elm.Gen.Basics
import Elm.Gen.Debug
import Elm.Gen.Dict
import Elm.Gen.Element as Element
import Elm.Gen.Element.Border as Border
import Elm.Gen.Element.Input as Input
import Elm.Gen.List
import Elm.Gen.List.Extra
import Elm.Gen.Maybe
import Elm.Gen.Result
import Elm.Gen.Set
import Elm.Gen.String
import Elm.Let
import Elm.Pattern
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Utils exposing (firstLower, firstUpper)


getFile : List (Result String TypeDecl) -> String
getFile typeDecls =
    let
        declarations =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToEditor

        defaults =
            typeDecls
                |> List.filterMap Result.toMaybe
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
            if List.isEmpty groups then
                ""

            else
                List.foldl
                    (\grouped str ->
                        str ++ "@docs " ++ String.join ", " grouped.members ++ "\n\n"
                    )
                    "\n\n"
                    groups
    in
    (Elm.fileWith [ "Editors" ]
        { docs = docs
        , aliases =
            [ ( [ "Element", "Input" ], "Input" )
            , ( [ "Element", "Border" ], "Border" )
            ]
        }
        (declarations ++ defaults ++ commonDeclarations)
    ).contents
        ++ comment


commonDeclarations : List Elm.Declaration
commonDeclarations =
    let
        rythmDeclaration =
            Elm.declaration "rythm" (Elm.int 10)

        rythm =
            Elm.valueWith [] "rythm" Elm.Annotation.int

        spacing =
            Elm.declaration "spacing" <| Element.spacing rythm

        padding =
            Elm.declaration "padding" <| Element.padding rythm
    in
    [ rythmDeclaration
    , spacing
    , padding
    , intEditor
    , stringEditor
    , boolEditor
    , listEditor
    , dictEditor
    ]


noLabel : Elm.Expression
noLabel =
    Input.labelHidden <| Elm.string ""


intEditor : Elm.Declaration
intEditor =
    Elm.fn "intEditor"
        ( "value", Elm.Annotation.int )
        (\value ->
            Elm.apply Element.id_.map
                [ Elm.lambda "newValue"
                    Elm.Annotation.string
                    (\newValue ->
                        newValue
                            |> Elm.pipe Elm.Gen.String.id_.toInt
                            |> Elm.pipe (Elm.apply Elm.Gen.Maybe.id_.withDefault [ value ])
                    )
                , Input.text [ Element.alignTop ]
                    { label = noLabel
                    , onChange = Elm.Gen.Basics.identity
                    , text = Elm.Gen.String.fromInt value
                    , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                    }
                ]
                |> Elm.withType (Element.types_.element Elm.Gen.Basics.types_.int)
        )


stringEditor : Elm.Declaration
stringEditor =
    Elm.fn "stringEditor"
        ( "value", Elm.Annotation.string )
        (\value ->
            Input.text [ Element.alignTop ]
                { label = noLabel
                , onChange = Elm.Gen.Basics.identity
                , text = value
                , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                }
                |> Elm.withType (Element.types_.element Elm.Gen.String.types_.string)
        )


boolEditor : Elm.Declaration
boolEditor =
    Elm.fn "boolEditor"
        ( "value", Elm.Annotation.bool )
        (\value ->
            Input.radioRow
                [ Elm.value "spacing"
                , Element.alignTop
                ]
                { label = noLabel
                , onChange = Elm.Gen.Basics.identity
                , options =
                    [ Input.option (Elm.value "True") <| Element.text <| Elm.string "True"
                    , Input.option (Elm.value "False") <| Element.text <| Elm.string "False"
                    ]
                , selected = Elm.Gen.Maybe.make_.maybe.just value
                }
                |> Elm.withType (Element.types_.element Elm.Gen.Basics.types_.bool)
        )


dictEditor : Elm.Declaration
dictEditor =
    let
        editorType t =
            Elm.Annotation.function [ t ] (Element.types_.element t)

        keyAnnotation =
            Elm.Annotation.var "comparable"

        valueAnnotation =
            Elm.Annotation.var "v"

        dictAnnotation =
            Elm.Annotation.dict keyAnnotation valueAnnotation
    in
    Elm.fn5 "dictEditor"
        ( "keyEditor", editorType keyAnnotation )
        ( "keyDefault", keyAnnotation )
        ( "valueEditor", editorType valueAnnotation )
        ( "valueDefault", valueAnnotation )
        ( "value", dictAnnotation )
        (\keyEditor keyDefault valueEditor valueDefault value ->
            let
                keysColumn =
                    Element.make_.column
                        { header = Element.none
                        , width = Element.shrink
                        , view = keysView
                        }

                valuesColumn =
                    Element.make_.column
                        { header = Element.none
                        , width = Element.fill
                        , view = valuesView
                        }

                keysView =
                    Elm.lambdaWith
                        [ ( Elm.Pattern.tuple (Elm.Pattern.var "key") (Elm.Pattern.var "memberValue")
                          , Elm.Annotation.tuple keyAnnotation valueAnnotation
                          )
                        ]
                        (let
                            key =
                                Elm.value "key"

                            memberValue =
                                Elm.value "memberValue"
                         in
                         Elm.apply Element.id_.map
                            [ Elm.lambda "newKey"
                                keyAnnotation
                                (\newKey ->
                                    Elm.ifThen
                                        (Elm.and
                                            (Elm.equal newKey keyDefault)
                                            (Elm.equal memberValue valueDefault)
                                        )
                                        (Elm.Gen.Dict.remove key value)
                                        (Elm.Gen.Dict.insert
                                            newKey
                                            memberValue
                                            (Elm.Gen.Dict.remove key value)
                                        )
                                )
                            , Elm.apply keyEditor [ key ]
                            ]
                        )

                valuesView =
                    Elm.lambdaWith
                        [ ( Elm.Pattern.tuple (Elm.Pattern.var "key") (Elm.Pattern.var "memberValue")
                          , Elm.Annotation.tuple keyAnnotation valueAnnotation
                          )
                        ]
                        (let
                            key =
                                Elm.value "key"

                            memberValue =
                                Elm.value "memberValue"
                         in
                         Elm.apply Element.id_.map
                            [ Elm.lambda "newValue"
                                keyAnnotation
                                (\newValue ->
                                    Elm.ifThen
                                        (Elm.and
                                            (Elm.equal key keyDefault)
                                            (Elm.equal newValue valueDefault)
                                        )
                                        (Elm.Gen.Dict.remove key value)
                                        (Elm.Gen.Dict.insert key newValue value)
                                )
                            , Elm.apply valueEditor [ memberValue ]
                            ]
                        )
            in
            Elm.letIn
                [ Elm.Let.value "keysColumn" keysColumn
                , Elm.Let.value "valuesColumn" valuesColumn
                ]
                (Elm.apply Element.id_.table
                    [ Elm.list
                        [ Elm.value "spacing"
                        , Elm.value "padding"
                        , Element.alignTop
                        , Border.width <| Elm.int 1
                        ]
                    , Elm.record
                        [ Elm.field "data"
                            (Elm.append
                                (Elm.Gen.Dict.toList value)
                                (Elm.list [ Elm.tuple keyDefault valueDefault ])
                            )
                        , Elm.field "columns" <|
                            Elm.list
                                [ Elm.value "keysColumn"
                                , Elm.value "valuesColumn"
                                ]
                        ]
                    ]
                    |> Elm.withType (Element.types_.element dictAnnotation)
                )
        )


listEditor : Elm.Declaration
listEditor =
    let
        editorType t =
            Elm.Annotation.function [ t ] (Element.types_.element t)

        valueAnnotation =
            Elm.Annotation.var "e"

        listAnnotation =
            Elm.Annotation.list valueAnnotation
    in
    Elm.fn3 "listEditor"
        ( "valueEditor", editorType valueAnnotation )
        ( "valueDefault", valueAnnotation )
        ( "value", listAnnotation )
        (\valueEditor valueDefault value ->
            let
                rows =
                    Elm.append
                        (Elm.Gen.List.indexedMap
                            (\i row ->
                                Elm.apply Element.id_.map
                                    [ Elm.lambda "newValue"
                                        valueAnnotation
                                        (\newValue ->
                                            Elm.apply Elm.Gen.List.Extra.id_.setAt
                                                [ i
                                                , newValue
                                                , value
                                                ]
                                        )
                                    , Elm.apply valueEditor [ row ]
                                    ]
                            )
                            value
                        )
                        (Elm.list
                            [ Elm.apply Element.id_.map
                                [ Elm.lambda "newValue"
                                    valueAnnotation
                                    (\newValue -> Elm.append value (Elm.list [ newValue ]))
                                , Elm.apply valueEditor [ valueDefault ]
                                ]
                            ]
                        )
            in
            Elm.letIn [ Elm.Let.value "rows" rows ]
                (Elm.apply Element.id_.column
                    [ Elm.list
                        [ Elm.value "spacing"
                        , Elm.value "padding"
                        , Element.alignTop
                        , Border.width <| Elm.int 1
                        ]
                    , Elm.value "rows"
                    ]
                    |> Elm.withType (Element.types_.element listAnnotation)
                )
        )


typeDeclToEditor : TypeDecl -> Elm.Declaration
typeDeclToEditor decl =
    let
        ( name, view ) =
            case decl of
                Alias n t ->
                    ( n, typeToEditor t )

                Custom n vs ->
                    ( n, customEditor n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name

        editorName =
            firstLower name ++ "Editor"

        declaration =
            (\value ->
                view value
                    |> Elm.withType (Element.types_.element tipe)
            )
                |> Elm.fn editorName ( "value", tipe )
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
        |> List.head
        |> Maybe.map
            (\( variantName, variantArgs ) ->
                variantArgs
                    |> List.map typeToDefault
                    |> Elm.apply (Elm.valueFrom [ "Model" ] variantName)
            )
        |> Maybe.withDefault
            ("It is not possible to generate a default for a custom type with no variants"
                |> Elm.string
                |> Elm.Gen.Debug.todo
            )


customEditor : String -> List Variant -> (Elm.Expression -> Elm.Expression)
customEditor typeName variants value =
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

        extractedPattern =
            extractedFields
                |> List.map Tuple.first
                |> Elm.Pattern.fields

        extractedValues =
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
                        ( Elm.Pattern.namedFrom [ "Model" ]
                            variantName
                            (List.map Elm.Pattern.var argNames)
                        , argNames
                            |> List.map (\name -> ( name ++ "Extracted", Elm.value name ))
                            |> Elm.updateRecord "extractedDefault"
                        )
                    )
                |> Elm.caseOf value

        variantRow =
            Input.radioRow [ Elm.value "spacing" ]
                { options = List.map variantToRadioOption variants
                , onChange = Elm.Gen.Basics.identity
                , selected = Elm.Gen.Maybe.make_.maybe.just value
                , label = noLabel
                }

        inputsRow =
            variants
                |> List.map variantToInputsRowCase
                |> Elm.caseOf value

        variantToRadioOption ( variantName, args ) =
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
                                ( Elm.value variable :: variables
                                , Dict.insert variableBasename newCount variablesCount
                                )
                            )
                            ( [], Dict.empty )
                        |> Tuple.first
                        |> List.reverse
            in
            Input.option
                (if List.isEmpty ctorArgs then
                    Elm.valueFrom [ "Model" ] variantName

                 else
                    Elm.apply (Elm.valueFrom [ "Model" ] variantName) ctorArgs
                )
                (Element.text <|
                    Elm.string <|
                        if String.startsWith typeName variantName then
                            String.dropLeft (String.length typeName) variantName

                        else
                            variantName
                )
    in
    if List.isEmpty extractedFields then
        Elm.letIn
            [ Elm.Let.value "variantRow" variantRow
            ]
            (Element.el
                [ Elm.value "padding"
                , Element.alignTop
                , Border.width <| Elm.int 1
                ]
                (Elm.value "variantRow")
            )

    else
        Elm.letIn
            [ Elm.Let.destructure extractedPattern extractedValues
            , Elm.Let.value "extractedDefault" extractedDefault
            , Elm.Let.value "variantRow" variantRow
            , Elm.Let.value "inputsRow" inputsRow
            ]
            (Element.column
                [ Elm.value "padding"
                , Elm.value "spacing"
                , Element.alignTop
                , Border.width <| Elm.int 1
                ]
                [ Elm.value "variantRow"
                , Elm.apply Element.id_.row [ Elm.list [ Elm.value "spacing" ], Elm.value "inputsRow" ]
                ]
            )


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


variantToInputsRowCase : ( String, List Type ) -> ( Elm.Pattern.Pattern, Elm.Expression )
variantToInputsRowCase ( variantName, args ) =
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
    ( Elm.Pattern.namedFrom [ "Model" ]
        variantName
        (List.map (Tuple.first >> Elm.Pattern.var) argNamesAndTypes)
    , argNamesAndTypes
        |> List.indexedMap
            (\i ( name, tipe ) ->
                Elm.apply Element.id_.map
                    [ Elm.lambda "newValue"
                        (typeToAnnotation tipe)
                        (\newValue ->
                            Elm.apply (Elm.valueFrom [ "Model" ] variantName)
                                (List.indexedMap
                                    (\j ( innerName, _ ) ->
                                        if i == j then
                                            newValue

                                        else
                                            Elm.value innerName
                                    )
                                    argNamesAndTypes
                                )
                        )
                    , typeToEditor tipe <| Elm.value name
                    ]
            )
        |> Elm.list
    )


todo : String -> Elm.Expression
todo =
    Elm.Gen.Debug.todo << Elm.string


typeToEditor : Type -> Elm.Expression -> Elm.Expression
typeToEditor =
    typeToEditorAndDefault >> Tuple.first


typeToDefault : Type -> Elm.Expression
typeToDefault =
    typeToEditorAndDefault >> Tuple.second


typeToEditorAndDefault : Type -> ( Elm.Expression -> Elm.Expression, Elm.Expression )
typeToEditorAndDefault tipe =
    let
        typeToEditorNameAndDefault =
            typeToEditorAndDefault
                >> Tuple.mapFirst (Elm.lambdaBetaReduced "value" (typeToAnnotation tipe))

        map ef df t1 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1
            in
            ( \value -> Elm.apply (Elm.value ef) [ e1, d1, value ]
            , df d1
            )

        map2 ef df t1 t2 =
            let
                ( e1, d1 ) =
                    typeToEditorNameAndDefault t1

                ( e2, d2 ) =
                    typeToEditorNameAndDefault t2
            in
            ( \value -> Elm.apply (Elm.value ef) [ e1, d1, e2, d2, value ]
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
            ( \value -> Elm.apply (Elm.value ef) [ e1, d1, e2, d2, e3, d3, value ]
            , df d1 d2 d3
            )
    in
    case tipe of
        Unit ->
            ( \_ -> Element.none, Elm.unit )

        Maybe inner ->
            map "maybeEditor" (always Elm.Gen.Maybe.make_.maybe.nothing) inner

        List inner ->
            map "listEditor" (always (Elm.list [])) inner

        Array inner ->
            map "arrayEditor" (always Elm.Gen.Array.empty) inner

        Set inner ->
            map "setEditor" (always Elm.Gen.Set.empty) inner

        Dict k v ->
            map2 "dictEditor" (\_ _ -> Elm.Gen.Dict.empty) k v

        Tuple a b ->
            map2 "tupleEditor" Elm.tuple a b

        Result e o ->
            map2 "resultEditor" (\_ okDefault -> Elm.Gen.Result.make_.result.ok okDefault) e o

        Triple a b c ->
            map3 "tripleEditor" Elm.triple a b c

        Object fields ->
            ( \value ->
                let
                    annotation =
                        typeToAnnotation tipe

                    data =
                        List.map
                            (\( fieldName, fieldType ) ->
                                Elm.tuple
                                    (Elm.string <| firstUpper fieldName)
                                    (Element.map
                                        (\newValue ->
                                            Elm.updateRecord "value" [ ( fieldName, newValue ) ]
                                        )
                                        (Elm.withType
                                            (Element.types_.element (typeToAnnotation fieldType))
                                            (typeToEditor fieldType
                                                (Elm.get fieldName value)
                                            )
                                        )
                                    )
                            )
                            fields

                    labelsColumn =
                        Element.make_.column
                            { header = Element.none
                            , width = Element.shrink
                            , view =
                                Elm.lambdaWith
                                    [ ( Elm.Pattern.tuple (Elm.Pattern.var "name") Elm.Pattern.wildcard
                                      , Elm.Annotation.tuple Elm.Annotation.string <| Element.types_.element annotation
                                      )
                                    ]
                                    (Element.text <| Elm.valueWith [] "name" Elm.Annotation.string)
                            }

                    inputColumn =
                        Element.make_.column
                            { header = Element.none
                            , width = Element.fill
                            , view =
                                Elm.lambdaWith
                                    [ ( Elm.Pattern.tuple Elm.Pattern.wildcard (Elm.Pattern.var "view")
                                      , Elm.Annotation.tuple Elm.Annotation.string <| Element.types_.element annotation
                                      )
                                    ]
                                    (Elm.value "view")
                            }
                in
                Element.table [ Elm.value "spacing" ]
                    { data = data
                    , columns = [ labelsColumn, inputColumn ]
                    }
            , fields
                |> List.map
                    (\( fieldName, fieldType ) ->
                        fieldType
                            |> typeToEditorAndDefault
                            |> Tuple.second
                            |> Elm.field fieldName
                    )
                |> Elm.record
            )

        Named n ->
            ( \value -> Elm.apply (Elm.value <| firstLower n ++ "Editor") [ value ]
            , case n of
                "String" ->
                    Elm.string ""

                "Char" ->
                    Elm.char ' '

                "Bool" ->
                    Elm.bool True

                "Int" ->
                    Elm.int 0

                "Float" ->
                    Elm.float 0

                _ ->
                    Elm.value <| firstLower n ++ "Default"
            )
