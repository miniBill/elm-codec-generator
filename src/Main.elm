module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Codec exposing (Codec)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, column, el, fill, height, inFront, padding, paddingEach, rgb, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import File exposing (File)
import File.Download
import File.Select
import List.Extra as List
import Task


type alias Flags =
    ()


type Msg
    = Edit Int (Maybe TypeDecl)
    | Download
    | Upload
    | Uploaded File
    | ReadFile String


type alias Model =
    Array TypeDecl


modelCodec : Codec Model
modelCodec =
    Codec.array typeDeclCodec


type TypeDecl
    = Alias String Type
    | Custom String (List Variant)


typeDeclCodec : Codec TypeDecl
typeDeclCodec =
    Codec.custom
        (\falias fcustom value ->
            case value of
                Alias a b ->
                    falias a b

                Custom a b ->
                    fcustom a b
        )
        |> Codec.variant2 "Alias" Alias Codec.string typeCodec
        |> Codec.variant2 "Custom" Custom Codec.string (Codec.list variantCodec)
        |> Codec.buildCustom


type alias Variant =
    ( String, List Type )


variantCodec : Codec Variant
variantCodec =
    Codec.tuple Codec.string (Codec.list typeCodec)


type Type
    = Record (List ( String, Type ))
    | Basic BasicType
    | Array Type
    | List Type
    | Dict Type Type
    | Named String


typeCodec : Codec Type
typeCodec =
    Codec.recursive
        (\child ->
            Codec.custom
                (\frecord fbasic flist farray fdict fnamed value ->
                    case value of
                        Record fields ->
                            frecord fields

                        Basic b ->
                            fbasic b

                        Array e ->
                            farray e

                        List e ->
                            flist e

                        Dict k v ->
                            fdict k v

                        Named n ->
                            fnamed n
                )
                |> Codec.variant1 "Record" Record (Codec.list (Codec.tuple Codec.string child))
                |> Codec.variant1 "Basic" Basic basicTypeCodec
                |> Codec.variant1 "Array" Array child
                |> Codec.variant1 "List" List child
                |> Codec.variant2 "Dict" Dict child child
                |> Codec.variant1 "Named" Named Codec.string
                |> Codec.buildCustom
        )


type BasicType
    = Int
    | Float
    | String


basicTypeCodec : Codec BasicType
basicTypeCodec =
    Codec.custom
        (\fint ffloat fstring value ->
            case value of
                Int ->
                    fint

                Float ->
                    ffloat

                String ->
                    fstring
        )
        |> Codec.variant0 "Int" Int
        |> Codec.variant0 "Float" Float
        |> Codec.variant0 "String" String
        |> Codec.buildCustom


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
    ( Array.empty, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit id type_ ->
            ( case type_ of
                Nothing ->
                    Array.removeAt id model

                Just t ->
                    if id == Array.length model then
                        Array.push t model

                    else
                        Array.set id t model
            , Cmd.none
            )

        Download ->
            ( model, File.Download.string "model.json" "application/json" <| Codec.encodeToString 0 modelCodec model )

        Upload ->
            ( model, File.Select.file [ "application/json" ] Uploaded )

        Uploaded file ->
            ( model, Task.perform ReadFile <| File.toString file )

        ReadFile file ->
            ( file
                |> Codec.decodeString modelCodec
                |> Result.withDefault model
            , Cmd.none
            )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Element Msg
view typeDecls =
    let
        children =
            typeDecls
                |> Array.toList
                |> (\l -> l ++ [ Alias "" <| Named "" ])
                |> List.indexedMap (\id t -> Element.map (Edit id) <| Element.Lazy.lazy viewTypeDecl t)
                |> wrappedRow
                    [ spacing rythm
                    , width fill
                    , height fill
                    ]
    in
    column [ spacing rythm, padding rythm, width fill, height fill ]
        [ wrappedRow [ spacing rythm ]
            [ Input.button [ Border.width 1, padding rythm ]
                { label = text "Download JSON"
                , onPress = Just Download
                }
            ]
        , children
        ]


rythm : Int
rythm =
    10


viewTypeDecl : TypeDecl -> Element (Maybe TypeDecl)
viewTypeDecl decl =
    let
        ( name, withName, ( aliasOption, customOption ) ) =
            case decl of
                Alias n t ->
                    ( n, \nn -> Alias nn t, ( decl, Custom n [] ) )

                Custom n vs ->
                    ( n, \nn -> Custom nn vs, ( Alias n (Basic Int), decl ) )
    in
    column
        [ Border.width 1
        , padding rythm
        , width fill
        , alignTop
        , spacing rythm
        , inFront <|
            Input.button
                [ alignRight
                , Border.widthEach { left = 1, bottom = 1, top = 0, right = 0 }
                , Border.color <| rgb 0 0 0
                , padding <| rythm * 3 // 4
                , Background.color <| rgb 1 0 0
                , Font.color <| rgb 1 1 1
                ]
                { label = text "X"
                , onPress = Just Nothing
                }
        ]
        [ Input.text [ width fill, spacing rythm ]
            { onChange = Just << withName
            , text = name
            , label = Input.labelAbove [] <| text "Name"
            , placeholder = Nothing
            }
        , Input.radioRow [ spacing rythm ]
            { label = Input.labelAbove [ paddingEach { top = 0, left = 0, right = 0, bottom = rythm } ] <| text "Kind"
            , onChange = Just
            , options =
                [ Input.option aliasOption <| text "Alias"
                , Input.option customOption <| text "Custom"
                ]
            , selected = Just decl
            }
        , case decl of
            Alias n t ->
                Element.map (Just << Alias n) (viewType t)

            Custom n vs ->
                Element.map (Just << Custom n) (editList viewVariant ( "", [] ) vs)
        , el [ Font.family [ Font.monospace ] ] <| text <| declToElm decl
        ]


viewVariant : Variant -> Element Variant
viewVariant ( name, args ) =
    row [ spacing rythm, Border.width 1, padding <| rythm // 2 ]
        [ Input.text [ alignTop ]
            { onChange = \newName -> ( newName, args )
            , text = name
            , placeholder = Nothing
            , label = Input.labelHidden "Name"
            }
        , Element.map (Tuple.pair name) <| editList viewType (Named "") args
        ]


editList : (t -> Element t) -> t -> List t -> Element (List t)
editList viewElement default list =
    Element.map (List.filterNot ((==) default)) <|
        column [ spacing rythm ] <|
            List.indexedMap (\i element -> Element.map (\new -> List.setAt i new list) <| viewElement element) list
                ++ [ Element.map (\new -> list ++ [ new ]) (viewElement default) ]


declToElm : TypeDecl -> String
declToElm decl =
    case decl of
        Alias n t ->
            "type alias "
                ++ n
                ++ " =\n"
                ++ indent 1 (typeToElm False t)
                ++ "\n\n\n"
                ++ firstLower n
                ++ "Codec : Codec "
                ++ n
                ++ "\n"
                ++ firstLower n
                ++ "Codec =\n"
                ++ indent 1 (typeToCodec False t)

        Custom n vs ->
            let
                variantToElm ( vn, va ) =
                    vn ++ " " ++ String.join " " (List.map (typeToElm True) va)
            in
            "type "
                ++ n
                ++ "\n"
                ++ indent 1 "= "
                ++ String.join "\n    | " (List.map variantToElm vs)
                ++ "\n\n\n"
                ++ firstLower n
                ++ "Codec : Codec "
                ++ n
                ++ "\n"
                ++ firstLower n
                ++ "Codec =\n"
                ++ customCodec vs


customCodec : List Variant -> String
customCodec variants =
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
            indent 2
                ("|> Codec.variant"
                    ++ String.fromInt (List.length args)
                    ++ " \""
                    ++ name
                    ++ "\" "
                    ++ name
                    ++ String.concat (List.map (\t -> " " ++ typeToCodec True t) args)
                    ++ "\n"
                )
    in
    indent 1 "Codec.custom\n"
        ++ indent 2
            ("(\\"
                ++ String.concat (List.map (\( name, _ ) -> "f" ++ firstLower name ++ " ") variants)
                ++ "value ->\n"
            )
        ++ indent 3 "case value of\n"
        ++ String.join "\n" (List.map variantToCase variants)
        ++ indent 2 ")\n"
        ++ String.concat (List.map variantToPipe variants)
        ++ indent 2 "|> Codec.buildCustom"


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


typeToElm : Bool -> Type -> String
typeToElm needParens t =
    let
        fieldToElm ( name, ft ) =
            name ++ " : " ++ typeToElm False ft

        parens r =
            if needParens then
                "(" ++ r ++ ")"

            else
                r
    in
    case t of
        Basic b ->
            basicToString b

        Record fs ->
            "{ " ++ String.join "\n    , " (List.map fieldToElm fs) ++ "\n    }"

        Array c ->
            parens <|
                "Array "
                    ++ typeToElm True c

        List c ->
            parens <|
                "List "
                    ++ typeToElm True c

        Dict k v ->
            parens <|
                "Dict "
                    ++ typeToElm True k
                    ++ " "
                    ++ typeToElm True v

        Named n ->
            if String.contains " " n then
                "( " ++ n ++ " )"

            else
                n


basicToString : BasicType -> String
basicToString b =
    case b of
        Int ->
            "Int"

        Float ->
            "Float"

        String ->
            "String"


typeToCodec : Bool -> Type -> String
typeToCodec needParens t =
    let
        parens r =
            if needParens then
                "(" ++ r ++ ")"

            else
                r
    in
    case t of
        Basic b ->
            "Codec." ++ firstLower (basicToString b)

        Record _ ->
            "Codec: branch 'Record _' not implemented"

        Array c ->
            parens <|
                "Codec.array "
                    ++ typeToCodec True c

        List c ->
            parens <|
                "Codec.list "
                    ++ typeToCodec True c

        Dict (Basic String) v ->
            parens <|
                "Codec.dict "
                    ++ typeToCodec True v

        Dict _ _ ->
            parens <|
                "Debug.todo \"Codecs for Dict with a non-string key are not supported\""

        Named n ->
            firstLower n ++ "Codec"


todo : String -> Element msg
todo msg =
    Element.text <| "TODO: " ++ msg


viewType : Type -> Element Type
viewType t =
    let
        default =
            { child = t
            , key = Basic String
            , fields = []
            , basic = Int
            , named = ""
            }

        { child, key, fields, basic, named } =
            case t of
                Array c ->
                    { default | child = c }

                List c ->
                    { default | child = c }

                Dict k c ->
                    { default | child = c, key = k }

                Record fs ->
                    { default | fields = fs }

                Basic b ->
                    { default | basic = b }

                Named n ->
                    { default | named = n }

        radio =
            Input.radioRow [ spacing rythm ]
                { label = Input.labelHidden "Kind"
                , onChange = identity
                , options =
                    [ Input.option (Array child) <| text "Array"
                    , Input.option (List child) <| text "List"
                    , Input.option (Dict key child) <| text "Dict"
                    , Input.option (Record fields) <| text "Record"
                    , Input.option (Basic basic) <| text "Basic"
                    , Input.option (Named named) <| text "Named"
                    ]
                , selected = Just t
                }

        leftPad =
            paddingEach { left = rythm * 2, top = 0, right = 0, bottom = 0 }

        details =
            case t of
                Record _ ->
                    todo "branch 'Record _' not implemented"

                Basic b ->
                    Input.radioRow [ spacing rythm, leftPad ]
                        { onChange = Basic
                        , options =
                            [ Int, String, Float ]
                                |> List.map (\o -> Input.option o <| text <| basicToString o)
                        , selected = Just b
                        , label = Input.labelHidden "Basic"
                        }

                Array c ->
                    el [ leftPad ] <| Element.map Array <| viewType c

                List c ->
                    el [ leftPad ] <| Element.map List <| viewType c

                Dict _ _ ->
                    todo "branch 'Dict _ _' not implemented"

                Named name ->
                    Input.text [ width fill ]
                        { label = Input.labelHidden "Name"
                        , text = name
                        , onChange = Named
                        , placeholder = Nothing
                        }
    in
    column [ spacing rythm ] [ radio, details ]
