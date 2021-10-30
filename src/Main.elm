module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Codec exposing (Codec)
import Dict exposing (Dict)
import Element exposing (Attribute, Element, alignRight, alignTop, column, el, fill, height, inFront, padding, paddingEach, px, rgb, row, shrink, spacing, text, width, wrappedRow)
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
    | Array Type
    | List Type
    | Dict Type Type
    | Named String
    | Tuple Type Type
    | Triple Type Type Type


typeCodec : Codec Type
typeCodec =
    Codec.recursive
        (\child ->
            Codec.custom
                (\frecord flist farray fdict fnamed ftuple ftriple value ->
                    case value of
                        Record fields ->
                            frecord fields

                        Array e ->
                            farray e

                        List e ->
                            flist e

                        Dict k v ->
                            fdict k v

                        Named n ->
                            fnamed n

                        Tuple a b ->
                            ftuple a b

                        Triple a b c ->
                            ftriple a b c
                )
                |> Codec.variant1 "Record" Record (Codec.list (Codec.tuple Codec.string child))
                |> Codec.variant1 "Array" Array child
                |> Codec.variant1 "List" List child
                |> Codec.variant2 "Dict" Dict child child
                |> Codec.variant1 "Named" Named Codec.string
                |> Codec.variant2 "Tuple" Tuple child child
                |> Codec.variant3 "Triple" Triple child child child
                |> Codec.buildCustom
        )


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
            , Input.button [ Border.width 1, padding rythm ]
                { label = text "Upload JSON"
                , onPress = Just Upload
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
                    ( n, \nn -> Custom nn vs, ( Alias n (Named ""), decl ) )
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
                Element.map (Just << Custom n) (editList wrappedRow viewVariant ( "", [] ) vs)
        , el [ Font.family [ Font.monospace ] ] <| text <| declToElm decl
        ]


viewVariant : Variant -> Element Variant
viewVariant ( name, args ) =
    column
        [ spacing rythm
        , Border.width 1
        , alignTop
        , padding <| rythm // 2
        ]
        [ Input.text [ alignTop, width <| Element.minimum 100 fill ]
            { onChange = \newName -> ( newName, args )
            , text = name
            , placeholder = Nothing
            , label = Input.labelHidden "Name"
            }
        , if String.isEmpty name then
            Element.none

          else
            Element.map (Tuple.pair name) <|
                editList columnWithHr viewType (Named "") args
        ]


columnWithHr : List (Attribute (List t)) -> List (Element (List t)) -> Element (List t)
columnWithHr attrs children =
    column attrs <| List.intersperse hr children


hr : Element msg
hr =
    el
        [ width fill
        , height <| px 1
        , Border.widthEach { top = 1, left = 0, right = 0, bottom = 0 }
        ]
        Element.none


editList :
    (List (Attribute (List t)) -> List (Element (List t)) -> Element (List t))
    -> (t -> Element t)
    -> t
    -> List t
    -> Element (List t)
editList container viewElement default list =
    Element.map (List.filterNot ((==) default)) <|
        container [ spacing rythm, alignTop ] <|
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

        Tuple a b ->
            "(" ++ typeToElm False a ++ ", " ++ typeToElm False b ++ ")"

        Triple a b c ->
            "(" ++ typeToElm False a ++ ", " ++ typeToElm False b ++ ", " ++ typeToElm False c ++ ")"


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
        Record fields ->
            indent 1
                ("Codec.object (\\"
                    ++ String.concat (List.map (\( n, _ ) -> n ++ " ") fields)
                    ++ "-> "
                )

        Array c ->
            parens <|
                "Codec.array "
                    ++ typeToCodec True c

        List c ->
            parens <|
                "Codec.list "
                    ++ typeToCodec True c

        Dict _ v ->
            parens <|
                "Codec.dict "
                    ++ typeToCodec True v

        Named n ->
            if isBasic n then
                "Codec." ++ firstLower n

            else
                firstLower n ++ "Codec"

        Tuple a b ->
            parens <| "Codec.tuple " ++ typeToCodec True a ++ " " ++ typeToCodec True b

        Triple _ _ _ ->
            parens "Debug.todo \"Codecs for triples are not supported\""


isBasic : String -> Bool
isBasic t =
    t == "String" || t == "Bool" || t == "Float" || t == "Int"


viewType : Type -> Element Type
viewType t =
    let
        default =
            { child = t
            , key = Named "String"
            , fields = []
            , named = ""
            , tuple0 = Named ""
            , tuple1 = Named ""
            , tuple2 = Named ""
            }

        { child, key, fields, named, tuple0, tuple1, tuple2 } =
            case t of
                Array c ->
                    { default | child = c }

                List c ->
                    { default | child = c }

                Dict k c ->
                    { default | child = c, key = k }

                Record fs ->
                    { default | fields = fs }

                Named n ->
                    { default | named = n }

                Tuple a b ->
                    { default | tuple0 = a, tuple1 = b }

                Triple a b c ->
                    { default | tuple0 = a, tuple1 = b, tuple2 = c }

        radio =
            Input.radioRow [ spacing rythm ]
                { label = Input.labelHidden "Kind"
                , onChange = identity
                , options =
                    [ Input.option (Array child) <| text "Arrary"
                    , Input.option (List child) <| text "List"
                    , Input.option (Dict key child) <| text "Dict"
                    , Input.option (Record fields) <| text "Record"
                    , Input.option (Named named) <| text "Named"
                    , Input.option (Tuple tuple0 tuple1) <| text "Tuple"
                    , Input.option (Triple tuple0 tuple1 tuple2) <| text "Triple"
                    ]
                , selected = Just t
                }

        leftPad =
            paddingEach { left = rythm * 2, top = 0, right = 0, bottom = 0 }

        details =
            case t of
                Record fs ->
                    let
                        viewField ( fn, ft ) =
                            row [ spacing rythm ]
                                [ Input.text [ width <| Element.minimum 100 shrink, alignTop ]
                                    { text = fn
                                    , onChange = \newName -> ( newName, ft )
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "Name"
                                    }
                                , Element.map (Tuple.pair fn) <| viewType ft
                                ]
                    in
                    Element.map Record <|
                        el [ leftPad ] <|
                            editList columnWithHr viewField ( "", Named "" ) fs

                Array c ->
                    el [ leftPad ] <| Element.map Array <| viewType c

                List c ->
                    el [ leftPad ] <| Element.map List <| viewType c

                Dict a b ->
                    column
                        [ spacing rythm
                        , leftPad
                        , Border.width 1
                        , padding <| rythm // 2
                        ]
                        [ Element.map (\newA -> Dict newA b) <| viewType a
                        , hr
                        , Element.map (\newB -> Dict a newB) <| viewType b
                        ]

                Named name ->
                    Input.text [ width fill ]
                        { label = Input.labelHidden "Name"
                        , text = name
                        , onChange = Named
                        , placeholder = Nothing
                        }

                Tuple a b ->
                    column
                        [ spacing rythm
                        , leftPad
                        , Border.width 1
                        , padding <| rythm // 2
                        ]
                        [ Element.map (\newA -> Tuple newA b) <| viewType a
                        , hr
                        , Element.map (\newB -> Tuple a newB) <| viewType b
                        ]

                Triple a b c ->
                    column
                        [ spacing rythm
                        , leftPad
                        , Border.width 1
                        , padding <| rythm // 2
                        ]
                        [ Element.map (\newA -> Triple newA b c) <| viewType a
                        , hr
                        , Element.map (\newB -> Triple a newB c) <| viewType b
                        , hr
                        , Element.map (\newC -> Triple a b newC) <| viewType c
                        ]
    in
    column [ spacing rythm, alignTop ] [ radio, details ]
