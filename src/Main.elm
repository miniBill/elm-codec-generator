port module Main exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Element exposing (Element, column, el, fill, height, paddingEach, paddingXY, px, scrollbarY, text, width, wrappedRow)
import Element.Font as Font
import Element.Input as Input
import Elm.CodeGen as Elm exposing (File)
import Elm.DSLParser
import Elm.Pretty
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Node as Node
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import File
import File.Download
import File.Select
import Result.Extra
import Task
import Theme


port save : Value -> Cmd msg


type alias Flags =
    Value


type Msg
    = Edit String
    | DownloadCodecs
    | Upload
    | Uploaded File.File
    | ReadFile String


type alias Model =
    { input : String
    , selectedTab : Tab
    }


modelCodec : Codec Model
modelCodec =
    Codec.object
        (\input selectedTab ->
            { input = input
            , selectedTab = selectedTab
            }
        )
        |> Codec.field "input" .input Codec.string
        |> Codec.field "selectedTab" .selectedTab tabCodec
        |> Codec.buildObject


type Tab
    = Codecs
    | Form


tabCodec : Codec Tab
tabCodec =
    Codec.custom
        (\fcodecs fform value ->
            case value of
                Codecs ->
                    fcodecs

                Form ->
                    fform
        )
        |> Codec.variant0 "Codecs" Codecs
        |> Codec.variant0 "Form" Form
        |> Codec.buildCustom


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
init stored =
    ( stored
        |> Codec.decodeValue modelCodec
        |> Result.withDefault
            { input = ""
            , selectedTab = Codecs
            }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, newCmd ) =
            case msg of
                Edit newInput ->
                    ( { model | input = newInput }, Cmd.none )

                DownloadCodecs ->
                    ( model
                    , File.Download.string "Codecs.elm" "application/elm" <|
                        getCodecsFile (parse model.input)
                    )

                Upload ->
                    ( model, File.Select.file [ "application/elm" ] Uploaded )

                Uploaded file ->
                    ( model, Task.perform ReadFile <| File.toString file )

                ReadFile file ->
                    ( { model | input = file }, Cmd.none )
    in
    ( newModel, Cmd.batch [ newCmd, save <| Codec.encodeToValue modelCodec newModel ] )


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
                    typeAnnotationToType typeAnnotation
                        |> Result.map
                            (Alias
                                (String.join " " <| List.map Node.value <| name :: generics)
                            )
                        |> addNameToError name
                        |> Just

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
                |> Result.Extra.combineMap
                    (\field ->
                        let
                            ( name, type_ ) =
                                Node.value field
                        in
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

        inner =
            if List.isEmpty generics then
                Result.Extra.combineMap (Node.value >> constructorToVariant) constructors
                    |> Result.map (Custom (Node.value name))

            else
                unsupported "generic types"
    in
    addNameToError name inner


addNameToError : Node.Node String -> Result String TypeDecl -> Result String TypeDecl
addNameToError name =
    Result.mapError (\e -> "Error generating Codec for " ++ Node.value name ++ ": " ++ e)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Element Msg
view model =
    let
        decls =
            parse model.input
    in
    column [ Theme.spacing, width fill, height fill ]
        [ wrappedRow [ Theme.spacing, paddingEach { left = Theme.rythm, top = Theme.rythm, bottom = 0, right = Theme.rythm } ]
            [ Theme.button
                { label = text "Upload File"
                , onPress = Just Upload
                }
            , Theme.button
                { label = text "Download Codecs"
                , onPress = Just DownloadCodecs
                }
            ]
        , el [ paddingXY Theme.rythm 0, width fill ] <|
            Input.multiline
                [ Theme.spacing
                , Theme.padding
                , width fill
                , height <| px 300
                , scrollbarY
                , Font.family [ Font.monospace ]
                ]
                { onChange = Edit
                , placeholder = Nothing
                , text = model.input
                , label = Input.labelAbove [ Font.family [ Font.sansSerif ] ] <| text "Input file"
                , spellcheck = False
                }
        , el
            [ Font.family [ Font.monospace ]
            , paddingEach { left = Theme.rythm, right = Theme.rythm, top = 0, bottom = Theme.rythm }
            , width fill
            , height fill
            , scrollbarY
            ]
            (text <| getCodecsFile decls)
        ]


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


firstLower : String -> String
firstLower n =
    case String.uncons n of
        Nothing ->
            ""

        Just ( h, tail ) ->
            String.cons (Char.toLower h) tail


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


isBasic : String -> Bool
isBasic t =
    t == "String" || t == "Bool" || t == "Float" || t == "Int"
