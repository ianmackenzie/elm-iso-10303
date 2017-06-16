module OpenSolid.Step.Decode
    exposing
        ( attribute
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

import List
import List.Extra as List
import OpenSolid.Step exposing (Attribute, Decoder, Entity, Header)
import OpenSolid.Step.Types as Types
import Parser


file : Decoder Entity a -> String -> Result String (List a)
file entityDecoder string =
    Err "Not implemented"


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
