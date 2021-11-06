module Editor exposing (getFile)

import Elm
import Elm.Annotation
import Elm.Gen.Basics
import Elm.Gen.Debug
import Elm.Gen.Dict
import Elm.Gen.Element as Element
import Elm.Gen.Element.Input as Input
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
        (declarations ++ commonDeclarations)
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
                , placeholder = Elm.value "Nothing"
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
                    ( n, typeToEditor n t )

                Custom n vs ->
                    ( n, customEditor n vs )

        tipe =
            Elm.Annotation.named [ "Model" ] name

        editorName =
            firstLower name ++ "Editor"

        declaration =
            Elm.fn editorName
                ( "value", tipe )
                (Elm.withType (Element.types_.element tipe) << view)
                |> Elm.expose
    in
    declaration


customEditor : String -> List Variant -> (Elm.Expression -> Elm.Expression)
customEditor typeName variants value =
    Element.column [] []


todo : String -> Elm.Expression
todo =
    Elm.Gen.Debug.todo << Elm.string


typeToEditor : String -> Type -> (Elm.Expression -> Elm.Expression)
typeToEditor name tipe value =
    case tipe of
        Unit ->
            Element.none

        Object fields ->
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
                                        (Elm.apply (typeToEditorName fieldType) [ Elm.get fieldName value ])
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

        _ ->
            Elm.apply (typeToEditorName tipe) [ value ]


typeToEditorName : Type -> Elm.Expression
typeToEditorName tipe =
    case tipe of
        Unit ->
            Elm.apply Elm.Gen.Basics.id_.always [ Element.none ]

        Maybe _ ->
            todo "branch 'Maybe _' not implemented"

        List inner ->
            Elm.apply (Elm.value "listEditor") [ typeToEditorName inner ]

        Array inner ->
            Elm.apply (Elm.value "arrayEditor") [ typeToEditorName inner ]

        Dict k v ->
            Elm.apply (Elm.value "dictEditor") [ typeToEditorName k, typeToEditorName v ]

        Set _ ->
            todo "branch 'Set _' not implemented"

        Tuple _ _ ->
            todo "branch 'Tuple _ _' not implemented"

        Triple _ _ _ ->
            todo "branch 'Triple _ _ _' not implemented"

        Result _ _ ->
            todo "branch 'Result _ _' not implemented"

        Object _ ->
            todo "branch 'Object _' not implemented"

        Named n ->
            Elm.value <| firstLower n ++ "Editor"
