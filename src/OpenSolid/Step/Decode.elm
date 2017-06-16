module OpenSolid.Step.Decode
    exposing
        ( Error(..)
        , attribute
        , bool
        , entity
        , fail
        , file
        , float
        , int
        , list
        , map
        , run
        , string
        , succeed
        , toAttribute
        , toEntity
        , tuple2
        , tuple3
        )

import Dict
import List
import List.Extra as List
import OpenSolid.Step exposing (Attribute, Decoder, Entity, Header)
import OpenSolid.Step.Parse as Parse
import OpenSolid.Step.Types as Types


type Error
    = ParseError Parse.Error
    | DecodeError String


collectDecodedValues : (i -> Maybe (Decoder i a)) -> List a -> List i -> Result String (List a)
collectDecodedValues decoderForInput collected inputs =
    case inputs of
        [] ->
            -- No more inputs to decode, so succeed with all the results we
            -- have collected so far
            Ok (List.reverse collected)

        first :: rest ->
            case decoderForInput first of
                Just (Types.Decoder decode) ->
                    -- There is a decoder for this input, so try decoding it
                    case decode first of
                        -- Decoding succeeded on this input: continue with the
                        -- rest
                        Ok result ->
                            collectDecodedValues decoderForInput
                                (result :: collected)
                                rest

                        -- Decoding failed on this input: immediately abort
                        -- with the returned error message
                        Err message ->
                            Err message

                Nothing ->
                    -- No decoder for this input, so skip it and continue with
                    -- the rest
                    collectDecodedValues decoderForInput collected rest


file : (Entity -> Maybe (Decoder Entity a)) -> String -> Result Error (List a)
file decoderForEntity fileContents =
    Parse.file fileContents
        |> Result.map (\( header, entities ) -> Dict.values entities)
        |> Result.mapError ParseError
        |> Result.andThen
            (\entities ->
                collectDecodedValues decoderForEntity [] entities
                    |> Result.mapError DecodeError
            )


run : Decoder i a -> i -> Result String a
run (Types.Decoder function) input =
    function input


succeed : a -> Decoder i a
succeed value =
    Types.Decoder (always (Ok value))


fail : String -> Decoder i a
fail description =
    Types.Decoder (always (Err description))


entity : a -> Decoder Entity a
entity constructor =
    Types.Decoder (always (Ok constructor))


attribute : Int -> Decoder Attribute a -> Decoder Entity (a -> b) -> Decoder Entity b
attribute index (Types.Decoder f) (Types.Decoder g) =
    Types.Decoder
        (\((Types.Entity _ attributes) as entity) ->
            case List.getAt index attributes of
                Just attribute ->
                    Result.map2 (|>) (f attribute) (g entity)

                Nothing ->
                    Err ("No attribute at index " ++ toString index)
        )


map : (a -> b) -> Decoder i a -> Decoder i b
map mapFunction (Types.Decoder function) =
    Types.Decoder (function >> Result.map mapFunction)


toEntity : Decoder Entity Entity
toEntity =
    Types.Decoder Ok


toAttribute : Decoder Attribute Attribute
toAttribute =
    Types.Decoder Ok


bool : Decoder Attribute Bool
bool =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.BoolAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a bool"
        )


int : Decoder Attribute Int
int =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.IntAttribute value ->
                    Ok value

                _ ->
                    Err "Expected an int"
        )


float : Decoder Attribute Float
float =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.FloatAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a float"
        )


string : Decoder Attribute String
string =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.StringAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a string"
        )


list : Decoder Attribute a -> Decoder Attribute (List a)
list itemDecoder =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.AttributeList attributes ->
                    collectDecodedValues
                        (always (Just itemDecoder))
                        []
                        attributes

                _ ->
                    Err "Expected a list"
        )


tuple2 : ( Decoder Attribute a, Decoder Attribute b ) -> Decoder Attribute ( a, b )
tuple2 ( firstDecoder, secondDecoder ) =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.AttributeList [ firstAttribute, secondAttribute ] ->
                    Result.map2 (,)
                        (run firstDecoder firstAttribute)
                        (run secondDecoder secondAttribute)

                _ ->
                    Err "Expected a list of two items"
        )


tuple3 : ( Decoder Attribute a, Decoder Attribute b, Decoder Attribute c ) -> Decoder Attribute ( a, b, c )
tuple3 ( firstDecoder, secondDecoder, thirdDecoder ) =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.AttributeList [ firstAttribute, secondAttribute, thirdAttribute ] ->
                    Result.map3 (,,)
                        (run firstDecoder firstAttribute)
                        (run secondDecoder secondAttribute)
                        (run thirdDecoder thirdAttribute)

                _ ->
                    Err "Expected a list of two items"
        )
