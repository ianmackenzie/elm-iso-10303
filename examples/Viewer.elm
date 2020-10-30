module Viewer exposing (main)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy
import File exposing (File)
import File.Select
import List.Extra
import Set exposing (Set)
import Step.Decode as Decode
import Step.Format as Format
import Step.TypeName as TypeName
import Step.Types as Step
import Task


type alias Model =
    { displayedEntities : Maybe (Result String (Hidden (List DisplayedEntity)))
    }


type Msg
    = LoadRequested
    | FileSelected File
    | FileLoaded String
    | SetDisplayedEntity Int DisplayedEntity


type alias Hidden a =
    () -> a


type DisplayedEntity
    = DisplayedEntity Step.Entity (Maybe DisplayedEntity)


unexpanded : Step.Entity -> DisplayedEntity
unexpanded entity =
    DisplayedEntity entity Nothing


hide : a -> Hidden a
hide value =
    \() -> value


reveal : Hidden a -> a
reveal hidden =
    hidden ()


init : () -> ( Model, Cmd Msg )
init () =
    ( { displayedEntities = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LoadRequested ->
            ( model, File.Select.file [] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded contents ->
            let
                parsedEntities =
                    Just <|
                        case Decode.file (Decode.allTopLevel Decode.identity) contents of
                            Ok entities ->
                                Ok (hide (List.map unexpanded entities))

                            Err (Decode.ParseError text) ->
                                Err ("STEP file parse error: " ++ text)

                            Err (Decode.NonexistentEntity id) ->
                                Err ("Nonexistent entity with id " ++ String.fromInt id ++ " referenced in STEP file")

                            Err (Decode.CircularReference ids) ->
                                Err ("Circular reference chain detected in STEP file: [" ++ String.join "," (List.map String.fromInt ids) ++ "]")

                            Err (Decode.DecodeError text) ->
                                Err ("STEP file decode error: " ++ text)
            in
            ( { model | displayedEntities = parsedEntities }, Cmd.none )

        SetDisplayedEntity index displayedEntity ->
            case model.displayedEntities of
                Nothing ->
                    ( model, Cmd.none )

                Just (Err _) ->
                    ( model, Cmd.none )

                Just (Ok displayedEntities) ->
                    let
                        currentDisplayedEntities =
                            reveal displayedEntities

                        updatedDisplayedEntities =
                            List.Extra.setAt index displayedEntity currentDisplayedEntities
                    in
                    ( { model | displayedEntities = Just (Ok (hide updatedDisplayedEntities)) }
                    , Cmd.none
                    )


viewEntityRecord : ( Step.TypeName, List Step.Attribute ) -> Element Step.Entity
viewEntityRecord ( typeName, attributes ) =
    Element.row []
        [ Element.text (String.toLower (TypeName.toString typeName))
        , Element.text "("
        , Element.row [] (List.map viewAttribute attributes |> List.intersperse (Element.text ","))
        , Element.text ")"
        ]


viewAttribute : Step.Attribute -> Element Step.Entity
viewAttribute attribute =
    case attribute of
        Step.DerivedValue ->
            Element.text Format.derivedValue

        Step.NullAttribute ->
            Element.text Format.null

        Step.BoolAttribute value ->
            Element.text (Format.bool value)

        Step.IntAttribute value ->
            Element.text (Format.int value)

        Step.FloatAttribute value ->
            Element.text (Format.float value)

        Step.StringAttribute value ->
            Element.text ("'" ++ value ++ "'")

        Step.BytesAttribute value ->
            Element.text (Format.bytes value)

        Step.EnumAttribute value ->
            Element.text (Format.enum value)

        Step.ReferenceTo entity ->
            Input.button
                [ Element.pointer
                , Element.Font.color (Element.rgb255 0 0 192)
                ]
                { onPress = Just entity
                , label = Element.text "#"
                }

        Step.TypedAttribute typeName nestedAttribute ->
            Element.row []
                [ Element.text (TypeName.toString typeName ++ "(")
                , viewAttribute nestedAttribute
                , Element.text ")"
                ]

        Step.AttributeList attributes ->
            Element.row []
                [ Element.text "("
                , Element.row []
                    (List.map viewAttribute attributes |> List.intersperse (Element.text ","))
                , Element.text ")"
                ]


toggleChild : Step.Entity -> DisplayedEntity -> DisplayedEntity
toggleChild child displayedEntity =
    case displayedEntity of
        DisplayedEntity parentEntity Nothing ->
            DisplayedEntity parentEntity (Just (unexpanded child))

        DisplayedEntity parentEntity (Just (DisplayedEntity currentChild _)) ->
            if child == currentChild then
                DisplayedEntity parentEntity Nothing

            else
                DisplayedEntity parentEntity (Just (unexpanded child))


viewEntity : DisplayedEntity -> Element DisplayedEntity
viewEntity ((DisplayedEntity parentEntity currentChild) as displayedEntity) =
    let
        parentEntityElement =
            Element.map (\referencedEntity -> toggleChild referencedEntity displayedEntity) <|
                case parentEntity of
                    Step.SimpleEntity typeName attributes ->
                        viewEntityRecord ( typeName, attributes )

                    Step.ComplexEntity entityRecords ->
                        Element.row []
                            [ Element.text "("
                            , Element.row [] (List.map viewEntityRecord entityRecords)
                            , Element.text ")"
                            ]
    in
    case currentChild of
        Nothing ->
            parentEntityElement

        Just child ->
            Element.column []
                [ parentEntityElement
                , Element.el [ Element.paddingEach { top = 0, bottom = 0, left = 16, right = 0 } ] <|
                    Element.map (\updatedChild -> DisplayedEntity parentEntity (Just updatedChild))
                        (viewEntity child)
                ]


viewTopLevelEntity : Int -> DisplayedEntity -> ( String, Element Msg )
viewTopLevelEntity index displayedEntity =
    ( String.fromInt index
    , viewEntity displayedEntity |> Element.map (SetDisplayedEntity index)
    )


view : Model -> Element Msg
view model =
    Element.column [ Element.Font.size 16, Element.width Element.fill ]
        [ Element.el [ Element.width Element.fill, Element.padding 4, Element.Background.color (Element.rgb255 192 192 192) ] <|
            Input.button [ Element.alignLeft ]
                { onPress = Just LoadRequested
                , label = Element.text "Load STEP file"
                }
        , case model.displayedEntities of
            Nothing ->
                Element.none

            Just (Ok displayedEntities) ->
                Element.Keyed.column []
                    (List.indexedMap viewTopLevelEntity (reveal displayedEntities))

            Just (Err text) ->
                Element.text text
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view >> Element.layout []
        }
