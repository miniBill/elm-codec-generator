module Editor exposing (getFile)

import Elm
import Elm.Annotation
import Elm.Gen.Array
import Elm.Gen.Basics
import Elm.Gen.Debug
import Elm.Gen.Dict
import Elm.Gen.Element as Element
import Elm.Gen.Element.Input as Input
import Elm.Gen.Maybe
import Elm.Gen.Result
import Elm.Gen.Set
import Elm.Gen.String
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
    , stringEditor
    , dictEditor
    ]


stringEditor : Elm.Declaration
stringEditor =
    Elm.fn "stringEditor"
        ( "value", Elm.Annotation.string )
        (\value ->
            Input.text []
                { label = Input.labelHidden <| Elm.string ""
                , onChange = Elm.Gen.Basics.identity
                , text = value
                , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
                }
                |> Elm.withType (Element.types_.element Elm.Gen.String.types_.string)
        )


dictEditor : Elm.Declaration
dictEditor =
    let
        editorType t =
            Elm.Annotation.function [ t ] (Element.types_.element t)

        keyAnnotation =
            Elm.Annotation.var "k"

        valueAnnotation =
            Elm.Annotation.var "v"

        dictAnnotation =
            Elm.Annotation.dict keyAnnotation valueAnnotation
    in
    Elm.fn3 "dictEditor"
        ( "keyEditor", editorType keyAnnotation )
        ( "valueEditor", editorType valueAnnotation )
        ( "value", dictAnnotation )
        (\keyEditor valueEditor value ->
            Elm.apply Element.id_.table
                [ Elm.list [ Elm.value "spacing" ]
                , Elm.record
                    [ Elm.field "data" <| Elm.Gen.Dict.toList value
                    , Elm.field "columns" <| Elm.Gen.Debug.todo <| Elm.string "TODO: dictEditor columns"
                    ]
                ]
                |> Elm.withType (Element.types_.element dictAnnotation)
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
                    |> Elm.apply (Elm.valueFrom [ "Module" ] variantName)
            )
        |> Maybe.withDefault (todo "It is not possible to generate a default for a custom type with no variants")


customEditor : String -> List Variant -> (Elm.Expression -> Elm.Expression)
customEditor typeName variants value =
    todo "customEditor"


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

                _ ->
                    Elm.value <| firstLower n ++ "Default"
            )
