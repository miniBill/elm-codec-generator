module Main exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, padding, paddingXY, px, scrollbarY, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download
import File.Select
import Parser exposing ((|.), (|=), Parser)
import Task


type alias Flags =
    ()


type Msg
    = Edit String
    | DownloadCodecs
    | Upload
    | Uploaded File
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
                getCodecsFile <|
                    parse model
            )

        Upload ->
            ( model, File.Select.file [ "application/elm" ] Uploaded )

        Uploaded file ->
            ( model, Task.perform ReadFile <| File.toString file )

        ReadFile file ->
            ( file, Cmd.none )


getCodecsFile : List TypeDecl -> String
getCodecsFile decls =
    decls
        |> List.map declToCodec
        |> (::) "module Codecs exposing (..)\n\nimport Codec exposing (Codec)"
        |> String.join "\n\n\n"


parse : String -> List TypeDecl
parse input =
    input
        |> String.split "\n\n\n"
        |> List.map (Parser.run declParser)
        |> List.filterMap Result.toMaybe


declParser : Parser TypeDecl
declParser =
    Parser.succeed identity
        |. Parser.keyword "type"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Alias
                |. Parser.keyword "alias"
                |. Parser.spaces
                |= nameParser
                |. Parser.spaces
                |. Parser.symbol "="
                |. Parser.spaces
                |= typeParser
            , Parser.succeed Custom
                |. Parser.spaces
                |= nameParser
                |. Parser.spaces
                |. Parser.symbol "="
                |= Parser.sequence
                    { start = ""
                    , end = ""
                    , item = variantParser
                    , spaces = Parser.spaces
                    , separator = "|"
                    , trailing = Parser.Optional
                    }
            ]
        |. Parser.spaces
        |. Parser.end


variantParser : Parser Variant
variantParser =
    Parser.succeed Tuple.pair
        |= nameParser
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , end = ""
            , item = typeParser
            , spaces = Parser.spaces
            , separator = " "
            , trailing = Parser.Optional
            }


nameParser : Parser String
nameParser =
    Parser.getChompedString <| Parser.chompWhile Char.isAlphaNum


typeParser : Parser Type
typeParser =
    Parser.lazy
        (\_ ->
            let
                oneChild name ctor =
                    Parser.succeed ctor
                        |. Parser.keyword name
                        |. Parser.spaces
                        |= typeParser

                twoChildren name ctor =
                    Parser.succeed ctor
                        |. Parser.keyword name
                        |. Parser.spaces
                        |= typeParser
                        |. Parser.spaces
                        |= typeParser

                threeChildren name ctor =
                    Parser.succeed ctor
                        |. Parser.keyword name
                        |. Parser.spaces
                        |= typeParser
                        |. Parser.spaces
                        |= typeParser
                        |. Parser.spaces
                        |= typeParser
            in
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.symbol "("
                    |. Parser.spaces
                    |= typeParser
                    |. Parser.spaces
                    |. Parser.symbol ")"
                , Parser.succeed Record
                    |= Parser.sequence
                        { start = "{"
                        , end = "}"
                        , trailing = Parser.Forbidden
                        , item =
                            Parser.succeed Tuple.pair
                                |= nameParser
                                |. Parser.spaces
                                |. Parser.symbol ":"
                                |. Parser.spaces
                                |= typeParser
                        , spaces = Parser.spaces
                        , separator = ","
                        }
                , oneChild "List" List
                , oneChild "Array" Array
                , oneChild "Maybe" Maybe
                , twoChildren "Dict" Dict
                , twoChildren "Result" Result
                , twoChildren "Tuple" Tuple
                , threeChildren "Triple" Triple
                , Parser.succeed Named
                    |= nameParser
                ]
        )


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


declToCodec : TypeDecl -> String
declToCodec decl =
    case decl of
        Alias n t ->
            let
                ( codec, isRecursive ) =
                    typeToCodec n False t
            in
            firstLower n
                ++ "Codec : Codec "
                ++ n
                ++ "\n"
                ++ firstLower n
                ++ "Codec =\n"
                ++ (if isRecursive then
                        indent 1
                            ("Codec.recursive (\\"
                                ++ firstLower n
                                ++ "RecursiveCodec ->\n"
                            )
                            ++ indent 2 codec
                            ++ indent 1 ")"

                    else
                        indent 1 codec
                   )

        Custom n vs ->
            let
                ( codec, isRecursive ) =
                    customCodec n vs
            in
            firstLower n
                ++ "Codec : Codec "
                ++ n
                ++ "\n"
                ++ firstLower n
                ++ "Codec =\n"
                ++ (if isRecursive then
                        indent 1
                            ("Codec.recursive (\\"
                                ++ firstLower n
                                ++ "RecursiveCodec ->\n"
                            )
                            ++ indent 2 codec
                            ++ indent 1 ")"

                    else
                        indent 1 codec
                   )


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
