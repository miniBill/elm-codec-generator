module Codecs exposing (getCodecsFile)

import Elm.CodeGen as Elm
import Elm.Pretty
import Model exposing (Type(..), TypeDecl(..), Variant)
import Utils exposing (firstLower)


isBasic : String -> Bool
isBasic t =
    t == "String" || t == "Bool" || t == "Float" || t == "Int"


typeToCodec : String -> Bool -> Type -> ( Elm.Expression, Bool )
typeToCodec typeName needParens t =
    let
        parens ( codec, recursive ) =
            if needParens then
                ( Elm.parens codec, recursive )

            else
                ( codec, recursive )

        oneChild ctor c =
            let
                ( childCodec, r ) =
                    typeToCodec typeName True c
            in
            parens <|
                ( Elm.apply
                    [ Elm.fqFun [ "Codec" ] ctor
                    , childCodec
                    ]
                , r
                )

        twoChildren ctor a b =
            let
                ( childCodecA, rA ) =
                    typeToCodec typeName True a

                ( childCodecB, rB ) =
                    typeToCodec typeName True b
            in
            parens <|
                ( Elm.apply
                    [ Elm.fqFun [ "Codec" ] ctor
                    , childCodecA
                    , childCodecB
                    ]
                , rA || rB
                )
    in
    case t of
        Record fields ->
            let
                ( fieldExprs, recursive ) =
                    fields
                        |> List.map
                            (\( fn, ft ) ->
                                case ft of
                                    Maybe it ->
                                        let
                                            ( childCodec, r ) =
                                                typeToCodec typeName True it
                                        in
                                        ( Elm.apply
                                            [ Elm.fqFun [ "Codec" ] "maybeField"
                                            , Elm.string fn
                                            , Elm.accessFun <| "." ++ fn
                                            , childCodec
                                            ]
                                        , r
                                        )

                                    _ ->
                                        let
                                            ( childCodec, r ) =
                                                typeToCodec typeName True ft
                                        in
                                        ( Elm.apply
                                            [ Elm.fqFun [ "Codec" ] "field"
                                            , Elm.string fn
                                            , Elm.accessFun <| "." ++ fn
                                            , childCodec
                                            ]
                                        , r
                                        )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            parens <|
                ( Elm.pipe
                    (Elm.apply
                        [ Elm.fqFun [ "Codec" ] "object"
                        , Elm.lambda
                            (List.map (\( n, _ ) -> Elm.varPattern n) fields)
                            (Elm.record
                                (List.map (\( n, _ ) -> ( n, Elm.val n )) fields)
                            )
                        ]
                    )
                    (fieldExprs
                        ++ [ Elm.fqFun [ "Codec" ] "buildObject" ]
                    )
                , recursive
                )

        Array c ->
            oneChild "array" c

        List c ->
            oneChild "list" c

        Maybe c ->
            oneChild "maybe" c

        Dict _ v ->
            oneChild "dict" v

        Result a b ->
            twoChildren "result" a b

        Tuple a b ->
            twoChildren "tuple" a b

        Unit ->
            parens
                ( Elm.apply
                    [ Elm.fqFun [ "Codec" ] "succeed"
                    , Elm.unit
                    ]
                , False
                )

        Triple _ _ _ ->
            parens
                ( Elm.apply
                    [ Elm.fqFun [ "Debug" ] "todo"
                    , Elm.string "Codecs for triples are not supported"
                    ]
                , False
                )

        Named n ->
            if isBasic n then
                ( Elm.fqVal [ "Codec" ] <| firstLower n, False )

            else if n == typeName then
                ( Elm.val <| firstLower n ++ "RecursiveCodec", not <| String.isEmpty n )

            else
                ( Elm.val <| firstLower n ++ "Codec", False )


customCodec : String -> List Variant -> ( Elm.Expression, Bool )
customCodec typeName variants =
    let
        variantToCase ( name, args ) =
            ( Elm.namedPattern name <|
                List.indexedMap
                    (\i _ -> Elm.varPattern <| "arg" ++ String.fromInt i)
                    args
            , Elm.apply
                ((Elm.fun <| "f" ++ firstLower name)
                    :: List.indexedMap (\i _ -> Elm.val <| "arg" ++ String.fromInt i) args
                )
            )

        variantToPipe ( name, args ) =
            let
                ( argsCodecs, argsAreRecursive ) =
                    args
                        |> List.map
                            (\t ->
                                let
                                    ( childCodec, r ) =
                                        typeToCodec typeName True t
                                in
                                ( childCodec, r )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            ( Elm.apply
                ([ Elm.fqFun [ "Codec" ] <| "variant" ++ String.fromInt (List.length args)
                 , Elm.string name
                 , Elm.fun name
                 ]
                    ++ argsCodecs
                )
            , argsAreRecursive
            )

        ( variantsCodecs, isRecursive ) =
            variants
                |> List.map variantToPipe
                |> List.unzip
                |> Tuple.mapSecond (List.any identity)
    in
    ( Elm.pipe
        (Elm.apply
            [ Elm.fqFun [ "Codec" ] "custom"
            , Elm.lambda
                (List.map (\( name, _ ) -> Elm.varPattern <| "f" ++ firstLower name) variants
                    ++ [ Elm.varPattern "value"
                       ]
                )
                (Elm.caseExpr (Elm.val "value") (List.map variantToCase variants))
            ]
        )
        (variantsCodecs
            ++ [ Elm.fqFun [ "Codec" ] "buildCustom" ]
        )
    , isRecursive
    )


typeDeclToCodecDeclaration : TypeDecl -> ( Elm.Declaration, Elm.TopLevelExpose )
typeDeclToCodecDeclaration decl =
    let
        ( name, ( codec, isRecursive ) ) =
            case decl of
                Alias n t ->
                    ( n, typeToCodec n False t )

                Custom n vs ->
                    ( n, customCodec n vs )

        expression =
            if isRecursive then
                Elm.apply
                    [ Elm.fqVal [ "Codec" ] "recursive"
                    , Elm.lambda
                        [ Elm.varPattern <| firstLower name ++ "RecursiveCodec"
                        ]
                        codec
                    ]

            else
                codec

        annotation =
            Elm.typed "Codec" [ Elm.typed name [] ]

        codecName =
            firstLower name ++ "Codec"

        declaration =
            Elm.valDecl Nothing
                (Just annotation)
                codecName
                expression

        expose =
            Elm.funExpose codecName
    in
    ( declaration, expose )


getCodecsFile : List (Result String TypeDecl) -> String
getCodecsFile typeDecls =
    let
        ( declarations, exposes ) =
            typeDecls
                |> List.filterMap Result.toMaybe
                |> List.map typeDeclToCodecDeclaration
                |> List.unzip

        errors =
            typeDecls
                |> List.filterMap
                    (\r ->
                        case r of
                            Err e ->
                                Just e

                            Ok _ ->
                                Nothing
                    )

        moduleDef =
            Elm.normalModule [ "Codecs" ] exposes

        imports =
            [ Elm.importStmt [ "Codec" ] Nothing (Just <| Elm.exposeExplicit [ Elm.closedTypeExpose "Codec" ])
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
