port module Main exposing (..)

import Dict
import Json.Encode exposing (Value)
import Kintail.Script as Script exposing (Allowed, Script)
import StepFile as Step
import StepFile.FastParse as Parse
import StepFile.File as StepFile
import Time exposing (Time)


script : List String -> Script { read : Allowed } Int ()
script arguments =
    case arguments of
        [ filename ] ->
            Script.readFile filename
                |> Script.onError (.message >> handleError)
                |> Script.andThen measureParseTime

        _ ->
            Script.print "Please supply the filename of one file to read"
                |> Script.andThen (\() -> Script.fail 1)


measureParseTime : String -> Script p Int ()
measureParseTime contents =
    Script.getCurrentTime
        |> Script.andThen
            (\startTime ->
                case Parse.file contents of
                    Ok file ->
                        printParseTime startTime file

                    Err parseError ->
                        handleError (toString parseError)
            )


printParseTime : Time -> Step.File -> Script p x ()
printParseTime startTime file =
    Script.getCurrentTime
        |> Script.andThen
            (\endTime ->
                let
                    entityCount =
                        StepFile.entities file |> Dict.size

                    elapsedTime =
                        Time.inSeconds (endTime - startTime)
                in
                Script.print
                    (toString entityCount
                        ++ " entities parsed in "
                        ++ toString elapsedTime
                        ++ " seconds"
                    )
            )


handleError : String -> Script p Int a
handleError message =
    Script.print ("ERROR: " ++ message)
        |> Script.andThen (\() -> Script.fail 1)


port requestPort : Value -> Cmd msg


port responsePort : (Value -> msg) -> Sub msg


main : Script.Program
main =
    Script.program script requestPort responsePort
