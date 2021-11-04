module Form exposing (getFile)

import Elm
import Elm.Annotation
import Elm.Gen.Basics
import Elm.Gen.Debug
import Elm.Gen.Element as Element
import Elm.Gen.Element.Input as Input
import Elm.Pattern
import FileParser exposing (typeToString)
import Model exposing (Type(..), TypeDecl(..), Variant, typeToAnnotation)
import Utils exposing (firstLower)


getFile : List (Result String TypeDecl) -> String
getFile typeDecls =
    let
        declarations =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToForm

        commonDeclarations =
            [ stringForm
            ]

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
    (Elm.fileWith [ "Forms" ]
        { docs = docs
        , aliases =
            [ ( [ "Element", "Input" ], "Input" )
            ]
        }
        (declarations ++ commonDeclarations)
    ).contents
        ++ comment


stringForm : Elm.Declaration
stringForm =
    Elm.fn "stringForm"
        ( "value", Elm.Annotation.string )
        (\value ->
            Input.text []
                { label = Input.labelHidden <| Elm.string ""
                , onChange = Elm.Gen.Basics.identity
                , text = value
                , placeholder = Elm.value "Nothing"
                }
        )


typeDeclToForm : TypeDecl -> Elm.Declaration
typeDeclToForm decl =
    let
        ( name, view ) =
            case decl of
                Alias n t ->
                    ( n, typeToForm n t )

                Custom n vs ->
                    ( n, customForm n vs )

        tipe =
            Elm.Annotation.named [] name

        editorName =
            firstLower name ++ "Editor"

        declaration =
            Elm.fn editorName
                ( "value", tipe )
                view
                |> Elm.expose
    in
    declaration


customForm : String -> List Variant -> (Elm.Expression -> Elm.Expression)
customForm arg1 arg2 arg3 =
    Elm.Gen.Debug.todo <| Elm.string "TODO: customForm"


typeToForm : String -> Type -> (Elm.Expression -> Elm.Expression)
typeToForm name type_ value =
    case type_ of
        Named "String" ->
            Elm.apply (Elm.value "stringForm") [ value ]

        Unit ->
            Element.none

        Object fields ->
            let
                tipe =
                    typeToAnnotation type_

                data =
                    [ Elm.Gen.Debug.todo <| Elm.string "data" ]

                column { header, width, view } =
                    Elm.record
                        [ Elm.field "header" header
                        , Elm.field "width" width
                        , Elm.field "view" view
                        ]

                labelsColumn =
                    column
                        { header = Element.none
                        , width = Element.shrink
                        , view =
                            Elm.lambdaWith
                                [ ( Elm.Pattern.tuple (Elm.Pattern.var "name") Elm.Pattern.wildcard
                                  , Elm.Annotation.tuple Elm.Annotation.string <| Element.types_.element tipe
                                  )
                                ]
                                (Element.text <| Elm.valueWith [] "name" Elm.Annotation.string)
                        }

                inputColumn =
                    column
                        { header = Element.none
                        , width = Element.fill
                        , view =
                            Elm.lambdaWith
                                [ ( Elm.Pattern.tuple Elm.Pattern.wildcard (Elm.Pattern.var "view")
                                  , Elm.Annotation.tuple Elm.Annotation.string <| Element.types_.element tipe
                                  )
                                ]
                                (Elm.value "view")
                        }
            in
            Element.table []
                { data = data
                , columns = [ labelsColumn, inputColumn ]
                }

        _ ->
            Elm.Gen.Debug.todo
                (Elm.string <| "TODO: typeToForm for " ++ typeToString False type_)
