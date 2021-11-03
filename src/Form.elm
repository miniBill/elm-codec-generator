module Form exposing (getFile)

import Elm
import Elm.Annotation
import Elm.Gen.Debug
import Elm.Gen.Element as Element
import Elm.Gen.Element.Input as Input
import Elm.Gen.Maybe
import FileParser exposing (typeToString)
import Model exposing (Type(..), TypeDecl(..), Variant)
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
    in
    (Elm.file [ "Forms" ] (commonDeclarations ++ declarations)).contents ++ comment


stringForm : Elm.Declaration
stringForm =
    Elm.fn "stringForm"
        ( "value", Elm.Annotation.string )
        (\value ->
            Input.text []
                { label = Input.labelHidden <| Elm.string ""
                , onChange = Elm.lambda "newValue" Elm.Annotation.string identity
                , text = value
                , placeholder = Elm.Gen.Maybe.make_.maybe.nothing
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

        _ ->
            Elm.Gen.Debug.todo
                (Elm.string <| "TODO: typeToForm for " ++ typeToString False type_)
