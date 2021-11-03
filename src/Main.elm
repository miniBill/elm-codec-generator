port module Main exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Codecs
import Element exposing (Element, column, el, fill, height, paddingEach, paddingXY, px, scrollbarY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File
import File.Download
import File.Select
import FileParser exposing (parse)
import Form
import Task
import Theme


port save : Value -> Cmd msg


type alias Flags =
    Value


type Msg
    = Edit String
    | DownloadCodecs
    | DownloadForms
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
                        Codecs.getFile (parse model.input)
                    )

                DownloadForms ->
                    ( model
                    , File.Download.string "Forms.elm" "application/elm" <|
                        Form.getFile (parse model.input)
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
                , onPress = Just DownloadCodecs
                }
            , Theme.button
                { label = text "Download Forms"
                , onPress = Just DownloadForms
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
                    , Border.widthEach { left = 1, right = 0, top = 1, bottom = 1 }
                    ]
                  , Codecs
                  )
                , ( "Form"
                  , [ Border.roundEach
                        { topRight = Theme.rythm
                        , topLeft = 0
                        , bottomRight = Theme.rythm
                        , bottomLeft = 0
                        }
                    , Border.widthEach { left = 1, right = 1, top = 1, bottom = 1 }
                    ]
                  , Form
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
            Codecs ->
                el
                    [ Font.family [ Font.monospace ]
                    , paddingEach { left = Theme.rythm, right = Theme.rythm, top = 0, bottom = Theme.rythm }
                    , width fill
                    , height fill
                    , scrollbarY
                    ]
                    (text <| Codecs.getFile decls)

            Form ->
                el
                    [ Font.family [ Font.monospace ]
                    , paddingEach { left = Theme.rythm, right = Theme.rythm, top = 0, bottom = Theme.rythm }
                    , width fill
                    , height fill
                    , scrollbarY
                    ]
                    (text <| Form.getFile decls)
        ]
