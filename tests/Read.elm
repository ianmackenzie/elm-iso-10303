module Read exposing (..)

import OpenSolid.Step.Parse as Parse
import RemoteData exposing (WebData)
import Dict
import Json.Decode as Decode
import Http
import Kintail.InputWidget as InputWidget
import Html exposing (Html)
import Html.Events


type alias StepData =
    WebData String


type alias Model =
    { fileName : String
    , stepData : StepData
    }


type Msg
    = FileNameEdited String
    | LoadRequested
    | DataReceived StepData


init : ( Model, Cmd Msg )
init =
    ( { fileName = "", stepData = RemoteData.NotAsked }, Cmd.none )


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
            in
                ( { model | stepData = RemoteData.Loading }
                , RemoteData.sendRequest request |> Cmd.map DataReceived
                )

        DataReceived stepData ->
            ( { model | stepData = stepData }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ InputWidget.lineEdit [] model.fileName |> Html.map FileNameEdited
            , Html.button [ Html.Events.onClick LoadRequested ]
                [ Html.text "Load" ]
            ]
        , Html.div []
            (case model.stepData of
                RemoteData.NotAsked ->
                    [ Html.text "" ]

                RemoteData.Loading ->
                    [ Html.text "Loading..." ]

                RemoteData.Failure _ ->
                    [ Html.text "Load failed" ]

                RemoteData.Success stepData ->
                    case Parse.file stepData of
                        Ok ( header, entities ) ->
                            [ Html.text
                                (toString (Dict.size entities) ++ " entities")
                            ]

                        Err (Parse.ParseError row column message) ->
                            [ Html.div []
                                [ Html.text
                                    ("Parse error at row "
                                        ++ toString row
                                        ++ ", column "
                                        ++ toString column
                                        ++ ": "
                                        ++ message
                                    )
                                ]
                            , Html.div []
                                [ Html.text stepData ]
                            ]

                        Err (Parse.ResolveError id) ->
                            [ Html.div []
                                [ Html.text
                                    ("Nonexistent entity with id "
                                        ++ toString id
                                        ++ " referenced"
                                    )
                                ]
                            , Html.div []
                                [ Html.text stepData ]
                            ]
            )
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
