module Form exposing (getFile)

import Elm.CodeGen as Elm
import Elm.Pretty
import Model exposing (Type(..), TypeDecl(..), Variant)
import Utils exposing (firstLower)


getFile : List (Result String TypeDecl) -> String
getFile typeDecls =
    let
        ( declarations, exposes ) =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToCodecDeclaration
                |> List.unzip

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

        moduleDef =
            Elm.normalModule [ "Forms" ] exposes

        imports =
            [ Elm.importStmt [ "Element" ]
                Nothing
                (Just <| Elm.exposeExplicit [ Elm.closedTypeExpose "Element" ])
            , Elm.importStmt [ "Element", "Input" ] (Just [ "Input" ]) Nothing
            , Elm.importStmt [ "Model" ] Nothing (Just <| Elm.exposeAll)
            ]

        comment =
            if List.isEmpty errors then
                Nothing

            else
                Just <| List.foldl Elm.markdown Elm.emptyFileComment errors
    in
    Elm.file moduleDef imports declarations comment
        |> Elm.Pretty.pretty 100


typeDeclToCodecDeclaration : TypeDecl -> ( Elm.Declaration, Elm.TopLevelExpose )
typeDeclToCodecDeclaration decl =
    let
        ( name, view ) =
            case decl of
                Alias n t ->
                    ( n, typeToForm n False t )

                Custom n vs ->
                    ( n, customForm n vs )

        type_ =
            Elm.typed name []

        annotation =
            Elm.funAnn type_ <|
                Elm.typed "Element" [ type_ ]

        editorName =
            firstLower name ++ "Editor"

        declaration =
            Elm.funDecl Nothing
                (Just annotation)
                editorName
                [ Elm.varPattern "value" ]
                view

        expose =
            Elm.funExpose editorName
    in
    ( declaration, expose )


customForm : String -> List Variant -> Elm.Expression
customForm arg1 arg2 =
    Elm.apply [ Elm.fqFun [ "Debug" ] "todo", Elm.string "TODO: customForm" ]


typeToForm : String -> Bool -> Type -> Elm.Expression
typeToForm name arg2 type_ =
    case type_ of
        Named "String" ->
            Elm.apply
                [ Elm.fqFun [ "Input" ] "text"
                , Elm.list []
                , Elm.record
                    [ ( "label"
                      , Elm.apply
                            [ Elm.fqFun [ "Input" ] "labelHidden"
                            , Elm.string name
                            ]
                      )
                    , ( "onChange", Elm.val "identity" )
                    , ( "text", Elm.val "value" )
                    , ( "placeholder", Elm.val "Nothing" )
                    ]
                ]

        Unit ->
            Elm.fqFun [ "Element" ] "none"

        _ ->
            Elm.apply [ Elm.fqFun [ "Debug" ] "todo", Elm.string "TODO: typeToForm" ]
