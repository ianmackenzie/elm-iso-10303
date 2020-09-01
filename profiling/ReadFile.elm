module ReadFile exposing (main)

import Duration
import Script exposing (Script)
import Script.File as File exposing (File, ReadOnly)
import Step.EntityResolution as EntityResolution
import Step.FastDecode as FastDecode


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


profile : File ReadOnly -> Script String ()
profile inputFile =
    Script.printLine (File.name inputFile)
        |> Script.andThen (File.read inputFile)
        |> Script.thenWith
            (\contents ->
                time "Preprocess" (\() -> Ok (FastDecode.preprocess contents))
            )
        |> Script.thenWith
            (\preprocessed ->
                time "Parse" (\() -> FastDecode.parse preprocessed)
                    |> Script.thenWith
                        (\parsedEntities ->
                            time "Resolve" (\() -> EntityResolution.resolve parsedEntities)
                                |> Script.onError
                                    (\resolutionError ->
                                        case resolutionError of
                                            EntityResolution.NonexistentEntity id ->
                                                case List.filter (Tuple.first >> (==) id) preprocessed.unparsedEntities of
                                                    [ _ ] ->
                                                        Script.fail ("Entity with ID " ++ String.fromInt id ++ "failed to parse")

                                                    _ ->
                                                        Script.fail ("No entity found with ID " ++ String.fromInt id)

                                            EntityResolution.CircularReference ids ->
                                                Script.fail ("Circular reference chain found: " ++ String.join "," (List.map String.fromInt ids))
                                    )
                        )
            )
        |> Script.ignoreResult
