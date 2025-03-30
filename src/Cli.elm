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
import Pages.Script.Spinner as Spinner


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
    Spinner.steps
        |> Spinner.withStep "Reading file" (\_ -> File.rawFile path |> BackendTask.allowFatal)
        |> Spinner.withStep "Parsing file" (\input -> FileParser.parse input |> BackendTask.succeed)
        |> Spinner.withStep "Generating codecs"
            (\{ moduleName, typeDecls } ->
                ( moduleName
                , Codecs.getFile
                    { moduleName = moduleName, optimizeDefaultFields = False }
                    typeDecls
                )
                    |> BackendTask.succeed
            )
        |> Spinner.withStep "Writing output"
            (\( moduleName, generated ) ->
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
        |> Spinner.runSteps
