port module Main exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Codecs
import Editors
import Element exposing (Element, column, el, fill, height, paddingEach, paddingXY, px, scrollbarY, text, width, wrappedRow)
import Element.Font as Font
import Element.Input as Input
import Elm.Syntax.ModuleName exposing (ModuleName)
import File
import File.Download
import File.Select
import FileParser exposing (parse)
import Task
import Theme
import Types exposing (TypeDecl)


port save : Value -> Cmd msg


type alias Flags =
    Value


type Msg
    = Edit String
    | DownloadCodecs { optimizeDefaultFields : Bool }
    | DownloadEditors
    | Upload
    | Uploaded File.File
    | ReadFile String


type alias Model =
    { input : String
    }


modelCodec : Codec Model
modelCodec =
    Codec.object
        (\input ->
            { input = input
            }
        )
        |> Codec.field "input" .input Codec.string
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
                    , let
                        parsed : { moduleName : ModuleName, typeDecls : List (Result String TypeDecl) }
                        parsed =
                            parse model.input
                      in
                      parsed.typeDecls
                        |> Codecs.getFile { moduleName = parsed.moduleName, optimizeDefaultFields = config.optimizeDefaultFields }
                        |> File.Download.string "Codecs.elm" "application/elm"
                    )

                DownloadEditors ->
                    ( model
                    , let
                        parsed : { moduleName : ModuleName, typeDecls : List (Result String TypeDecl) }
                        parsed =
                            parse model.input
                      in
                      parsed.typeDecls
                        |> Editors.getFile { moduleName = parsed.moduleName, optimizeDefaultFields = False }
                        |> File.Download.string "Editors.elm" "application/elm"
                    )

                Upload ->
                    ( model, File.Select.file [ "application/elm" ] Uploaded )

                Uploaded file ->
                    ( model, Task.perform ReadFile <| File.toString file )

                ReadFile file ->
                    ( { model | input = file }, Cmd.none )
    in
    ( newModel, Cmd.batch [ newCmd, save <| Codec.encodeToValue modelCodec newModel ] )


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Element Msg
view model =
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
        ]
