module ReadFile exposing (main)

import Dict
import Duration
import Regex
import Script exposing (Script)
import Script.File as File exposing (File, ReadOnly)
import Step.EntityResolution as EntityResolution
import Step.FastParse as FastParse
import Step.Types


main : Script.Program
main =
    Script.program script


script : Script.Init -> Script String ()
script { arguments, userPrivileges } =
    arguments
        |> Script.each
            (\fileName ->
                let
                    inputFile =
                        File.readOnly userPrivileges fileName
                in
                profile inputFile
            )


time : String -> (() -> Result x a) -> Script x a
time tag givenFunction =
    Script.getCurrentTime
        |> Script.thenWith
            (\startTime ->
                case givenFunction () of
                    Ok result ->
                        Script.getCurrentTime
                            |> Script.thenWith
                                (\endTime ->
                                    let
                                        elapsed =
                                            Duration.from startTime endTime
                                    in
                                    Script.printLine ("  " ++ tag ++ ": " ++ String.fromFloat (Duration.inSeconds elapsed) ++ " s")
                                        |> Script.andThen (Script.succeed result)
                                )

                    Err error ->
                        Script.fail error
            )


handleError : Script Step.Types.Error a -> Script String a
handleError =
    Script.mapError
        (\error ->
            case error of
                Step.Types.ParseError message ->
                    "Parse error: " ++ message

                Step.Types.NonexistentEntity id ->
                    "No valid entity found with ID " ++ String.fromInt id

                Step.Types.CircularReference ids ->
                    "Circular reference chain found: " ++ String.join "," (List.map String.fromInt ids)

                Step.Types.DecodeError message ->
                    "Decode error: " ++ message
        )


profile : File ReadOnly -> Script String ()
profile inputFile =
    Script.printLine (File.name inputFile)
        |> Script.andThen (File.read inputFile)
        |> Script.thenWith
            (\contents ->
                time "Preprocess" (\() -> Ok (FastParse.preprocess contents))
                    |> Script.thenWith
                        (\preprocessed ->
                            time "Parse" (\() -> FastParse.postprocess preprocessed)
                                |> handleError
                                |> Script.thenWith
                                    (\parsed ->
                                        time "Resolve" (\() -> EntityResolution.resolve parsed.entities)
                                            |> handleError
                                            |> Script.aside
                                                (\_ ->
                                                    Script.printLine ("  File timestamp: " ++ parsed.header.timeStamp)
                                                )
                                            |> Script.thenWith
                                                (\entityDict ->
                                                    let
                                                        actualCount =
                                                            Dict.size entityDict

                                                        expectedCount =
                                                            Regex.find (Regex.fromString "#\\d+\\s*=" |> Maybe.withDefault Regex.never) contents
                                                                |> List.length

                                                        expectedString =
                                                            if actualCount == expectedCount then
                                                                "as expected"

                                                            else
                                                                "expected: " ++ String.fromInt expectedCount
                                                    in
                                                    Script.printLine ("  " ++ String.fromInt actualCount ++ " entities loaded (" ++ expectedString ++ ")")
                                                )
                                    )
                        )
            )
