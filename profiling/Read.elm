module Read exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element
import Element.Input as Input
import Html exposing (Html)
import Http
import Process
import RemoteData exposing (RemoteData)
import StepFile
import Task exposing (Task)
import Time


type alias StepFile =
    ( List ( Int, String ), Array String )


type ParseState
    = Unparsed String
    | Parsed ( Float, StepFile )


type alias Model =
    { fileName : String
    , stepData : RemoteData String ParseState
    }


type Msg
    = FileNameEdited String
    | LoadRequested
    | DataReceived String
    | LoadFailed
    | FileParsed ( Float, StepFile )
    | ParseFailed String


init : ( Model, Cmd Msg )
init =
    ( { fileName = "", stepData = RemoteData.NotAsked }, Cmd.none )


chainToString : List Int -> String
chainToString chain =
    "[" ++ String.join "," (List.map String.fromInt chain) ++ "]"


parse : String -> Task String ( Float, StepFile )
parse string =
    Process.sleep 100
        |> Task.andThen
            (\() ->
                Time.now
                    |> Task.andThen
                        (\startTime ->
                            case Step.parse string of
                                Ok file ->
                                    Time.now
                                        |> Task.map
                                            (\finishTime ->
                                                let
                                                    duration =
                                                        toFloat (Time.posixToMillis finishTime - Time.posixToMillis startTime) / 1000
                                                in
                                                ( duration, file )
                                            )

                                Err (Step.SyntaxError message) ->
                                    Task.fail
                                        ("Syntax error: " ++ message)

                                Err (Step.NonexistentEntity id) ->
                                    Task.fail
                                        ("Nonexistent entity with id "
                                            ++ String.fromInt id
                                            ++ " referenced"
                                        )

                                Err (Step.CircularReference chain) ->
                                    Task.fail
                                        ("Circular reference detected: "
                                            ++ chainToString chain
                                        )
                        )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FileNameEdited fileName ->
            ( { fileName = fileName, stepData = RemoteData.NotAsked }
            , Cmd.none
            )

        LoadRequested ->
            let
                request =
                    Http.request
                        { method = "GET"
                        , headers = [ Http.header "Accept" "text/plain" ]
                        , url = model.fileName
                        , body = Http.emptyBody
                        , expect = Http.expectString
                        , timeout = Nothing
                        , withCredentials = False
                        }

                handleResult result =
                    case result of
                        Ok string ->
                            DataReceived string

                        Err _ ->
                            LoadFailed
            in
            ( { model | stepData = RemoteData.Loading }
            , Http.send handleResult request
            )

        DataReceived string ->
            let
                handleResult result =
                    case result of
                        Ok file ->
                            FileParsed file

                        Err errorMessage ->
                            ParseFailed errorMessage

                parseCmd =
                    Task.attempt handleResult (parse string)
            in
            ( { model | stepData = RemoteData.Success (Unparsed string) }
            , parseCmd
            )

        LoadFailed ->
            ( { model | stepData = RemoteData.Failure "Could not load file" }
            , Cmd.none
            )

        FileParsed file ->
            ( { model | stepData = RemoteData.Success (Parsed file) }
            , Cmd.none
            )

        ParseFailed errorMessage ->
            ( { model | stepData = RemoteData.Failure errorMessage }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column []
            [ Input.text []
                { onChange = Just FileNameEdited
                , text = model.fileName
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "File to load")
                }
            , Input.button []
                { onPress = Just LoadRequested
                , label = Element.text "Load"
                }
            , Element.text <|
                case model.stepData of
                    RemoteData.NotAsked ->
                        ""

                    RemoteData.Loading ->
                        "Loading..."

                    RemoteData.Failure message ->
                        message

                    RemoteData.Success (Unparsed _) ->
                        "Parsing..."

                    RemoteData.Success (Parsed ( time, ( entityLines, strings ) )) ->
                        String.fromInt (List.length entityLines)
                            ++ " entities, parsed in "
                            ++ String.fromFloat time
                            ++ " seconds"
            ]


main : Program () Model Msg
main =
    Browser.embed
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
