port module Main exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Codecs
import Editor
import Element exposing (Element, column, el, fill, height, paddingEach, paddingXY, px, scrollbarY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import FileParser exposing (parse)
import Task
import Theme


port save : Value -> Cmd msg


type alias Flags =
    Value


type Msg
    = Edit String
    | DownloadCodecs Codecs.Config
    | DownloadEditors
    | Upload
    | Uploaded File.File
    | ReadFile String
    | SelectTab Tab


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
    = Codecs Codecs.Config
    | Editor


tabCodec : Codec Tab
tabCodec =
    Codec.custom
        (\fcodecs feditor value ->
            case value of
                Codecs c ->
                    fcodecs c

                Editor ->
                    feditor
        )
        |> Codec.variant1 "Codecs" Codecs codecsConfigCodec
        |> Codec.variant0 "Editor" Editor
        |> Codec.buildCustom


codecsConfigCodec : Codec Codecs.Config
codecsConfigCodec =
    Codec.object
        (\optimizeDefaultFields ->
            { optimizeDefaultFields = optimizeDefaultFields
            }
        )
        |> Codec.field "optimizeDefaultFields" .optimizeDefaultFields Codec.bool
        |> Codec.buildObject


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
            , selectedTab = Codecs { optimizeDefaultFields = False }
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

                DownloadCodecs config ->
                    ( model
                    , File.Download.string "Codecs.elm" "application/elm" <|
                        Codecs.getFile config (parse model.input)
                    )

                DownloadEditors ->
                    ( model
                    , File.Download.string "Editors.elm" "application/elm" <|
                        Editor.getFile (parse model.input)
                    )

                Upload ->
                    ( model, File.Select.file [ "application/elm" ] Uploaded )

                Uploaded file ->
                    ( model, Task.perform ReadFile <| File.toString file )

                ReadFile file ->
                    ( { model | input = file }, Cmd.none )

                SelectTab tab ->
                    ( { model | selectedTab = tab }, Cmd.none )
    in
    ( newModel, Cmd.batch [ newCmd, save <| Codec.encodeToValue modelCodec newModel ] )


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
                , onPress = Just <| DownloadCodecs { optimizeDefaultFields = False }
                }
            , Theme.button
                { label = text "Download optimized Codecs"
                , onPress = Just <| DownloadCodecs { optimizeDefaultFields = True }
                }
            , Theme.button
                { label = text "Download Editors"
                , onPress = Just DownloadEditors
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
        , Input.radioRow [ paddingXY Theme.rythm 0 ]
            { options =
                [ ( "Codecs"
                  , [ Border.roundEach
                        { topLeft = Theme.rythm
                        , topRight = 0
                        , bottomLeft = Theme.rythm
                        , bottomRight = 0
                        }
                    , Border.width 1
                    ]
                  , Codecs { optimizeDefaultFields = False }
                  )
                , ( "Optimized Codecs"
                  , [ Border.widthXY 0 1
                    ]
                  , Codecs { optimizeDefaultFields = True }
                  )
                , ( "Editors"
                  , [ Border.roundEach
                        { topRight = Theme.rythm
                        , topLeft = 0
                        , bottomRight = Theme.rythm
                        , bottomLeft = 0
                        }
                    , Border.width 1
                    ]
                  , Editor
                  )
                ]
                    |> List.map
                        (\( label, attrs, option ) ->
                            Input.optionWith option
                                (\state ->
                                    el
                                        (attrs
                                            ++ [ Theme.padding
                                               , Background.color <|
                                                    case state of
                                                        Input.Idle ->
                                                            Element.rgb 1 1 1

                                                        Input.Selected ->
                                                            Element.rgb 0.9 0.9 0.9

                                                        Input.Focused ->
                                                            Element.rgb 1 1 1
                                               ]
                                        )
                                        (text label)
                                )
                        )
            , selected = Just model.selectedTab
            , onChange = SelectTab
            , label = Input.labelHidden "Selected tab"
            }
        , case model.selectedTab of
            Codecs config ->
                el
                    [ Font.family [ Font.monospace ]
                    , paddingEach { left = Theme.rythm, right = Theme.rythm, top = 0, bottom = Theme.rythm }
                    , width fill
                    , height fill
                    , scrollbarY
                    ]
                    (text <| Codecs.getFile config decls)

            Editor ->
                el
                    [ Font.family [ Font.monospace ]
                    , paddingEach { left = Theme.rythm, right = Theme.rythm, top = 0, bottom = Theme.rythm }
                    , width fill
                    , height fill
                    , scrollbarY
                    ]
                    (text <| Editor.getFile decls)
        ]
