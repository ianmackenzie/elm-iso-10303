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


collectDecodedEntities : (Entity -> Maybe (Decoder Entity a)) -> List a -> List ( Int, Entity ) -> Result String (List a)
collectDecodedEntities decoderForEntity collected entities =
    case entities of
        [] ->
            -- No more entities to decode, so succeed with all the results we
            -- have collected so far
            Ok (List.reverse collected)

        ( id, entity ) :: rest ->
            case decoderForEntity entity of
                Just decoder ->
                    -- There is a decoder for this entity, so try decoding it
                    case run decoder entity of
                        -- Decoding succeeded on this entity: continue with the
                        -- rest
                        Ok result ->
                            collectDecodedEntities decoderForEntity
                                (result :: collected)
                                rest

                        -- Decoding failed on this entity: immediately abort
                        -- with the returned error message
                        Err message ->
                            Err
                                ("In entity #"
                                    ++ toString id
                                    ++ ": "
                                    ++ message
                                )

                Nothing ->
                    -- No decoder for this entity, so skip it and continue with
                    -- the rest
                    collectDecodedEntities decoderForEntity collected rest


file : (Entity -> Maybe (Decoder Entity a)) -> String -> Result Error (List a)
file decoderForEntity fileContents =
    Parse.file fileContents
        |> Result.map (\( header, entities ) -> Dict.toList entities)
        |> Result.mapError ParseError
        |> Result.andThen
            (\entities ->
                collectDecodedEntities decoderForEntity [] entities
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
attribute index attributeDecoder entityDecoder =
    Types.Decoder
        (\((Types.Entity typeName attributes) as entity) ->
            case List.getAt index attributes of
                Just attribute ->
                    case run attributeDecoder attribute of
                        Ok value ->
                            run entityDecoder entity
                                |> Result.map
                                    (\constructor -> constructor value)

                        Err message ->
                            Err
                                ("At attribute index "
                                    ++ toString index
                                    ++ ": "
                                    ++ message
                                )

                Nothing ->
                    Err ("No attribute at index " ++ toString index)
        )


map : (a -> b) -> Decoder i a -> Decoder i b
map mapFunction (Types.Decoder function) =
    Types.Decoder (function >> Result.map mapFunction)


mapError : (String -> String) -> Decoder i a -> Decoder i a
mapError mapFunction (Types.Decoder function) =
    Types.Decoder (function >> Result.mapError mapFunction)


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


collectDecodedAttributes : Decoder Attribute a -> List a -> List Attribute -> Result String (List a)
collectDecodedAttributes decoder collected attributes =
    case attributes of
        [] ->
            -- No more attributes to decode, so succeed with all the results we
            -- have collected so far
            Ok (List.reverse collected)

        first :: rest ->
            case run decoder first of
                -- Decoding succeeded on this attribute: continue with the
                -- rest
                Ok result ->
                    collectDecodedAttributes decoder (result :: collected) rest

                -- Decoding failed on this attribute: immediately abort
                -- with the returned error message
                Err message ->
                    Err message


list : Decoder Attribute a -> Decoder Attribute (List a)
list itemDecoder =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.AttributeList attributes ->
                    collectDecodedAttributes itemDecoder [] attributes

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
                    Err "Expected a list of three items"
        )
        )
