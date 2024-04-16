module Cli exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.OptionsParser.BuilderState exposing (AnyOptions)
import Cli.Program as Program
import Codecs
import FatalError exposing (FatalError)
import FileParser
import Pages.Script as Script exposing (Script)


type alias CliOptions =
    { path : String
    , out : Maybe String
    }


run : Script
run =
    Script.withCliOptions program task


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add optionsParser


optionsParser : OptionsParser.OptionsParser CliOptions AnyOptions
optionsParser =
    OptionsParser.build CliOptions
        |> OptionsParser.with
            (Option.requiredPositionalArg "path")
        |> OptionsParser.with
            (Option.optionalKeywordArg "out")


task : CliOptions -> BackendTask FatalError ()
task { path, out } =
    File.rawFile path
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\input ->
                let
                    { moduleName, typeDecls } =
                        FileParser.parse input

                    generated : String
                    generated =
                        Codecs.getFile { moduleName = moduleName, optimizeDefaultFields = False } typeDecls
                in
                case out of
                    Nothing ->
                        Script.log generated

                    Just outPath ->
                        Script.writeFile
                            { path = String.join "/" (outPath :: moduleName) ++ ".elm"
                            , body = generated
                            }
                            |> BackendTask.allowFatal
            )
