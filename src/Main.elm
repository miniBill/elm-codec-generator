module Main exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, padding, paddingXY, px, scrollbarY, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elm.CodeGen as Elm exposing (File)
import Elm.DSLParser
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import File
import File.Download
import File.Select
import Result.Extra
import Task


type alias Flags =
    ()


type Msg
    = Edit String
    | DownloadCodecs
    | Upload
    | Uploaded File.File
    | ReadFile String


type alias Model =
    String


type TypeDecl
    = Alias String Type
    | Custom String (List Variant)


type alias Variant =
    ( String, List Type )


type Type
    = Record (List ( String, Type ))
    | Array Type
    | List Type
    | Dict Type Type
    | Named String
    | Unit
    | Tuple Type Type
    | Triple Type Type Type
    | Maybe Type
    | Result Type Type


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Element.layout [] << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd msg )
init _ =
    ( "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit newModel ->
            ( newModel, Cmd.none )

        DownloadCodecs ->
            ( model
            , File.Download.string "Codecs.elm" "application/elm" <|
                getCodecsFile (parse model)
            )

        Upload ->
            ( model, File.Select.file [ "application/elm" ] Uploaded )

        Uploaded file ->
            ( model, Task.perform ReadFile <| File.toString file )

        ReadFile file ->
            ( file, Cmd.none )


getCodecsFile : List (Result String TypeDecl) -> String
getCodecsFile decls =
    decls
        |> List.map declToCodec
        |> (::) "module Codecs exposing (..)\n\nimport Codec exposing (Codec)\nimport Model exposing (..)"
        |> String.join "\n\n\n"


parse : String -> List (Result String TypeDecl)
parse input =
    case Elm.DSLParser.parse input of
        Err _ ->
            [ Err "Error parsing file. If you want to have a more detailed error, feel free to open a PR ;)" ]

        Ok o ->
            fileToTypeDecls o


fileToTypeDecls : File -> List (Result String TypeDecl)
fileToTypeDecls { declarations } =
    let
        declarationToTypeDecl decl =
            let
                inner =
                    case decl of
                        Elm.DeclNoComment i ->
                            i

                        Elm.DeclWithComment _ f ->
                            f ""
            in
            case inner of
                Declaration.AliasDeclaration { name, generics, typeAnnotation } ->
                    Just <|
                        Result.map
                            (Alias
                                (String.join " " <| List.map Node.value <| name :: generics)
                            )
                            (typeAnnotationToType typeAnnotation)

                Declaration.CustomTypeDeclaration t ->
                    Just <| customTypeToTypeDecl t

                _ ->
                    Nothing
    in
    List.filterMap declarationToTypeDecl declarations


typeAnnotationToType : Node.Node TypeAnnotation.TypeAnnotation -> Result String Type
typeAnnotationToType tyan =
    case Node.value tyan of
        TypeAnnotation.Typed ctor args ->
            let
                ( mod, name ) =
                    Node.value ctor
            in
            if List.isEmpty mod then
                case ( name, Result.Extra.combineMap typeAnnotationToType args ) of
                    ( _, Err e ) ->
                        Err e

                    ( _, Ok [] ) ->
                        Ok <| Named name

                    ( "Dict", Ok [ k, v ] ) ->
                        Ok <| Dict k v

                    ( "Result", Ok [ e, o ] ) ->
                        Ok <| Result e o

                    ( "List", Ok [ i ] ) ->
                        Ok <| List i

                    ( "Array", Ok [ i ] ) ->
                        Ok <| Array i

                    ( "Maybe", Ok [ i ] ) ->
                        Ok <| Maybe i

                    ( _, Ok ts ) ->
                        Err <|
                            "Codec generation not supported for "
                                ++ String.join " " (name :: List.map (typeToString True) ts)

            else
                Err <| "Qualified names, like " ++ String.join "." (mod ++ [ name ]) ++ ", are not supported"

        TypeAnnotation.Record fields ->
            fields
                |> List.map Node.value
                |> Result.Extra.combineMap
                    (\( name, type_ ) ->
                        Result.map
                            (Tuple.pair <| Node.value name)
                            (typeAnnotationToType type_)
                    )
                |> Result.map Record

        TypeAnnotation.Unit ->
            Ok Unit

        TypeAnnotation.Tupled args ->
            case Result.Extra.combineMap typeAnnotationToType args of
                Err e ->
                    Err e

                Ok [] ->
                    Ok Unit

                Ok [ l, r ] ->
                    Ok <| Tuple l r

                Ok [ l, m, r ] ->
                    Ok <| Triple l m r

                Ok _ ->
                    unsupported "tuple of more than three arguments"

        TypeAnnotation.GenericType _ ->
            unsupported "generic types"

        TypeAnnotation.GenericRecord _ _ ->
            unsupported "generic records"

        TypeAnnotation.FunctionTypeAnnotation _ _ ->
            unsupported "function types"


unsupported : String -> Result String x
unsupported kind =
    Err <| "Codec generation not supported for " ++ kind


typeToString : Bool -> Type -> String
typeToString needParens t =
    let
        fieldToElm ( name, ft ) =
            name ++ " : " ++ typeToString False ft

        parens r =
            if needParens then
                "(" ++ r ++ ")"

            else
                r
    in
    case t of
        Record fs ->
            "{ " ++ String.join "\n    , " (List.map fieldToElm fs) ++ "\n    }"

        Array c ->
            parens <|
                "Array "
                    ++ typeToString True c

        List c ->
            parens <|
                "List "
                    ++ typeToString True c

        Maybe c ->
            parens <|
                "Maybe "
                    ++ typeToString True c

        Result k v ->
            parens <|
                "Result "
                    ++ typeToString True k
                    ++ " "
                    ++ typeToString True v

        Dict k v ->
            parens <|
                "Dict "
                    ++ typeToString True k
                    ++ " "
                    ++ typeToString True v

        Named n ->
            if String.contains " " n then
                "( " ++ n ++ " )"

            else
                n

        Unit ->
            "()"

        Tuple a b ->
            "(" ++ typeToString False a ++ ", " ++ typeToString False b ++ ")"

        Triple a b c ->
            "(" ++ typeToString False a ++ ", " ++ typeToString False b ++ ", " ++ typeToString False c ++ ")"


customTypeToTypeDecl : Type.Type -> Result String TypeDecl
customTypeToTypeDecl { name, generics, constructors } =
    let
        constructorToVariant ctor =
            Result.map
                (Tuple.pair <| Node.value ctor.name)
                (Result.Extra.combineMap typeAnnotationToType ctor.arguments)
    in
    if List.isEmpty generics then
        Result.map
            (Custom (Node.value name))
            (Result.Extra.combineMap (Node.value >> constructorToVariant) constructors)

    else
        unsupported "generic types"


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Element Msg
view file =
    let
        decls =
            parse file
    in
    column [ spacing rythm, paddingXY 0 rythm, width fill, height fill ]
        [ wrappedRow [ spacing rythm, paddingXY rythm 0 ]
            [ Input.button [ Border.width 1, padding rythm ]
                { label = text "Upload File"
                , onPress = Just Upload
                }
            , Input.button [ Border.width 1, padding rythm ]
                { label = text "Download Codecs"
                , onPress = Just DownloadCodecs
                }
            ]
        , el [ paddingXY rythm 0, width fill ] <|
            Input.multiline
                [ spacing rythm
                , padding rythm
                , width fill
                , height <| px 300
                , scrollbarY
                , Font.family [ Font.monospace ]
                ]
                { onChange = Edit
                , placeholder = Nothing
                , text = file
                , label = Input.labelAbove [] <| text "Input file"
                , spellcheck = False
                }
        , el
            [ Font.family [ Font.monospace ]
            , paddingXY rythm 0
            , width fill
            , height fill
            , scrollbarY
            ]
            (text <| getCodecsFile decls)
        ]


rythm : Int
rythm =
    10


declToCodec : Result String TypeDecl -> String
declToCodec res =
    case res of
        Err e ->
            "-- ERROR: " ++ e

        Ok decl ->
            let
                ( name, ( codec, isRecursive ) ) =
                    case decl of
                        Alias n t ->
                            ( n, typeToCodec n False t )

                        Custom n vs ->
                            ( n, customCodec n vs )

                inner =
                    if isRecursive then
                        indent 1
                            ("Codec.recursive (\\"
                                ++ firstLower name
                                ++ "RecursiveCodec ->\n"
                            )
                            ++ indent 2 codec
                            ++ indent 1 ")"

                    else
                        indent 1 codec
            in
            firstLower name
                ++ "Codec : Codec "
                ++ name
                ++ "\n"
                ++ firstLower name
                ++ "Codec =\n"
                ++ inner


customCodec : String -> List Variant -> ( String, Bool )
customCodec typeName variants =
    let
        variantToCase ( name, args ) =
            indent 4
                (name
                    ++ String.concat (List.indexedMap (\i _ -> " arg" ++ String.fromInt i) args)
                    ++ " ->\n"
                )
                ++ indent 5
                    ("f"
                        ++ firstLower name
                        ++ String.concat (List.indexedMap (\i _ -> " arg" ++ String.fromInt i) args)
                        ++ "\n"
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
                                ( " " ++ childCodec, r )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            ( indent 2
                ("|> Codec.variant"
                    ++ String.fromInt (List.length args)
                    ++ " \""
                    ++ name
                    ++ "\" "
                    ++ name
                    ++ String.concat argsCodecs
                    ++ "\n"
                )
            , argsAreRecursive
            )

        ( variantsCodecs, isRecursive ) =
            variants
                |> List.map variantToPipe
                |> List.unzip
                |> Tuple.mapSecond (List.any identity)
    in
    ( indent 1 "Codec.custom\n"
        ++ indent 2
            ("(\\"
                ++ String.concat (List.map (\( name, _ ) -> "f" ++ firstLower name ++ " ") variants)
                ++ "value ->\n"
            )
        ++ indent 3 "case value of\n"
        ++ String.join "\n" (List.map variantToCase variants)
        ++ indent 2 ")\n"
        ++ String.concat variantsCodecs
        ++ indent 2 "|> Codec.buildCustom"
    , isRecursive
    )


indent : Int -> String -> String
indent i s =
    String.repeat i "    " ++ s


firstLower : String -> String
firstLower n =
    case String.uncons n of
        Nothing ->
            ""

        Just ( h, tail ) ->
            String.cons (Char.toLower h) tail


typeToCodec : String -> Bool -> Type -> ( String, Bool )
typeToCodec typeName needParens t =
    let
        parens ( codec, recursive ) =
            if needParens then
                ( "(" ++ codec ++ ")", recursive )

            else
                ( codec, recursive )

        oneChild ctor c =
            let
                ( childCodec, r ) =
                    typeToCodec typeName True c
            in
            parens <|
                ( "Codec."
                    ++ ctor
                    ++ " "
                    ++ childCodec
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
                ( "Codec."
                    ++ ctor
                    ++ " "
                    ++ childCodecA
                    ++ " "
                    ++ childCodecB
                , rA || rB
                )
    in
    case t of
        Record fields ->
            let
                ( fieldsStrings, recursive ) =
                    fields
                        |> List.map
                            (\( fn, ft ) ->
                                case ft of
                                    Maybe it ->
                                        let
                                            ( childCodec, r ) =
                                                typeToCodec typeName True it
                                        in
                                        ( indent 2 <| "|> Codec.maybeField \"" ++ fn ++ "\" ." ++ fn ++ " " ++ childCodec ++ "\n", r )

                                    _ ->
                                        let
                                            ( childCodec, r ) =
                                                typeToCodec typeName True ft
                                        in
                                        ( indent 2 <| "|> Codec.field \"" ++ fn ++ "\" ." ++ fn ++ " " ++ childCodec ++ "\n", r )
                            )
                        |> List.unzip
                        |> Tuple.mapSecond (List.any identity)
            in
            parens <|
                ( "Codec.object\n"
                    ++ indent 2 "(\\"
                    ++ String.concat (List.map (\( n, _ ) -> n ++ " ") fields)
                    ++ "->\n"
                    ++ indent 3 "{ "
                    ++ String.join (indent 3 ", ")
                        (List.map (\( n, _ ) -> n ++ " = " ++ n ++ "\n") fields)
                    ++ indent 3 "}\n"
                    ++ indent 2 ")\n"
                    ++ String.concat fieldsStrings
                    ++ indent 2 "|> Codec.buildObject"
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
            parens ( "Codec.succeed ()", False )

        Triple _ _ _ ->
            parens ( "Debug.todo \"Codecs for triples are not supported\"", False )

        Named n ->
            if isBasic n then
                ( "Codec." ++ firstLower n, False )

            else if n == typeName then
                ( firstLower n ++ "RecursiveCodec", not <| String.isEmpty n )

            else
                ( firstLower n ++ "Codec", False )


isBasic : String -> Bool
isBasic t =
    t == "String" || t == "Bool" || t == "Float" || t == "Int"
