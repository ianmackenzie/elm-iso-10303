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
        , map
        , run
        , string
        , succeed
        , toAttribute
        , toEntity
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


collectDecodedValues : (Entity -> Maybe (Decoder Entity a)) -> List a -> List Entity -> Result Error (List a)
collectDecodedValues decoderForEntity collected entities =
    case entities of
        [] ->
            -- No more entities to decode, so succeed with all the results we
            -- have collected so far
            Ok (List.reverse collected)

        first :: rest ->
            case decoderForEntity first of
                Just (Types.Decoder decode) ->
                    -- There is a decoder for this entity, so try decoding
                    case decode first of
                        -- Decoding succeeded on this entity: continue with the
                        -- rest
                        Ok result ->
                            collectDecodedValues
                                decoderForEntity
                                (result :: collected)
                                rest

                        -- Decoding failed on this entity: immediately abort
                        -- with the returned error message
                        Err message ->
                            Err (DecodeError message)

                Nothing ->
                    -- No decoder for this entity, so skip it and continue with
                    -- the rest
                    collectDecodedValues decoderForEntity collected rest


file : (Entity -> Maybe (Decoder Entity a)) -> String -> Result Error (List a)
file decoderForEntity fileContents =
    Parse.file fileContents
        |> Result.map (\( header, entities ) -> Dict.values entities)
        |> Result.mapError ParseError
        |> Result.andThen (collectDecodedValues decoderForEntity [])


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
