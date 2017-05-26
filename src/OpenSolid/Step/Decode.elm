module OpenSolid.Step.Decode
    exposing
        ( attribute
        , bool
        , decodeAttribute
        , decodeEntity
        , decodeFile
        , entity
        , fail
        , float
        , int
        , map
        , string
        , succeed
        , toAttribute
        , toEntity
        )

import List
import List.Extra as List
import OpenSolid.Step exposing (Attribute, Decoder, Entity, Header)
import OpenSolid.Step.Types as Types
import Parser


decodeFile : Decoder Entity a -> String -> Result String (List a)
decodeFile entityDecoder string =
    Err "Not implemented"


decodeEntity : Decoder Entity a -> Entity -> Result String a
decodeEntity (Decoder function) entity =
    function entity


decodeAttribute : Decoder Attribute a -> Attribute -> Result String a
decodeAttribute (Decoder function) attribute =
    function attribute


succeed : a -> Decoder i a
succeed value =
    Decoder (always (Ok value))


fail : String -> Decoder i a
fail description =
    Decoder (always (Err description))


entity : a -> Decoder Entity a
entity value =
    Decoder (always (Ok value))


attribute : Int -> Decoder Attribute a -> Decoder Entity (a -> b) -> Decoder Entity b
attribute index (Decoder f) (Decoder g) =
    Decoder
        (\((Types.Entity _ attributes) as entity) ->
            case List.getAt index attributes of
                Just attribute ->
                    Result.map2 (|>) (f attribute) (g entity)

                Nothing ->
                    Err ("No attribute at index " ++ toString index)
        )


map : (a -> b) -> Decoder i a -> Decoder i b
map mapFunction (Decoder function) =
    Decoder (function >> Result.map mapFunction)


toEntity : Decoder Entity Entity
toEntity =
    Decoder Ok


toAttribute : Decoder Attribute Attribute
toAttribute =
    Decoder Ok


bool : Decoder Attribute Bool
bool =
    Decoder
        (\attribute ->
            case attribute of
                Types.BoolAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a bool"
        )


int : Decoder Attribute Int
int =
    Decoder
        (\attribute ->
            case attribute of
                Types.IntAttribute value ->
                    Ok value

                _ ->
                    Err "Expected an int"
        )


float : Decoder Attribute Float
float =
    Decoder
        (\attribute ->
            case attribute of
                Types.FloatAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a float"
        )


string : Decoder Attribute String
string =
    Decoder
        (\attribute ->
            case attribute of
                Types.StringAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a string"
        )
