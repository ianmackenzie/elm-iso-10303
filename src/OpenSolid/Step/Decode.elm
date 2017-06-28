module OpenSolid.Step.Decode
    exposing
        ( andThen
        , attribute
        , bool
        , default
        , entity
        , fail
        , file
        , float
        , int
        , lazy
        , list
        , map
        , null
        , oneOf
        , optional
        , referenced
        , run
        , string
        , succeed
        , toAttribute
        , toEntity
        , tuple2
        , tuple3
        , withDefault
        )

import Bitwise
import Dict
import List
import List.Extra as List
import OpenSolid.Step exposing (Attribute, Decoder, Entity, File, Header)
import OpenSolid.Step.Parse as Parse
import OpenSolid.Step.Types as Types
import Parser exposing ((|.), (|=), Parser)
import String.Extra as String


run : Decoder i a -> i -> Result String a
run (Types.Decoder function) input =
    function input


succeed : a -> Decoder i a
succeed value =
    Types.Decoder (always (Ok value))


fail : String -> Decoder i a
fail description =
    Types.Decoder (always (Err description))


file : a -> Decoder File a
file constructor =
    succeed constructor


entity : a -> Decoder Entity a
entity constructor =
    succeed constructor


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


isBasic : Char -> Bool
isBasic character =
    character /= '\'' && character /= '\\'


hexDigit : Parser Int
hexDigit =
    Parser.oneOf
        [ Parser.symbol "0" |> Parser.map (\() -> 0)
        , Parser.symbol "1" |> Parser.map (\() -> 1)
        , Parser.symbol "2" |> Parser.map (\() -> 2)
        , Parser.symbol "3" |> Parser.map (\() -> 3)
        , Parser.symbol "4" |> Parser.map (\() -> 4)
        , Parser.symbol "5" |> Parser.map (\() -> 5)
        , Parser.symbol "6" |> Parser.map (\() -> 6)
        , Parser.symbol "7" |> Parser.map (\() -> 7)
        , Parser.symbol "8" |> Parser.map (\() -> 8)
        , Parser.symbol "9" |> Parser.map (\() -> 9)
        , Parser.symbol "A" |> Parser.map (\() -> 10)
        , Parser.symbol "B" |> Parser.map (\() -> 11)
        , Parser.symbol "C" |> Parser.map (\() -> 12)
        , Parser.symbol "D" |> Parser.map (\() -> 13)
        , Parser.symbol "E" |> Parser.map (\() -> 14)
        , Parser.symbol "F" |> Parser.map (\() -> 15)
        ]


x0 : Int -> Int -> String
x0 high low =
    String.fromCodePoints [ Bitwise.shiftLeftBy 4 high + low ]


x2 : List ( Int, Int, Int, Int ) -> String
x2 hexDigits =
    let
        codePoint ( a, b, c, d ) =
            d
                + Bitwise.shiftLeftBy 4 c
                + Bitwise.shiftLeftBy 8 b
                + Bitwise.shiftLeftBy 12 a
    in
    String.fromCodePoints (List.map codePoint hexDigits)


x4 : List ( Int, Int, Int, Int, Int, Int ) -> String
x4 hexDigits =
    let
        codePoint ( a, b, c, d, e, f ) =
            f
                + Bitwise.shiftLeftBy 4 e
                + Bitwise.shiftLeftBy 8 d
                + Bitwise.shiftLeftBy 12 c
                + Bitwise.shiftLeftBy 16 b
                + Bitwise.shiftLeftBy 20 a
    in
    String.fromCodePoints (List.map codePoint hexDigits)


decodeString : Parser String
decodeString =
    Parser.succeed String.concat
        |= Parser.repeat Parser.zeroOrMore
            (Parser.oneOf
                [ Parser.symbol "''" |> Parser.map (\() -> "'")
                , Parser.symbol "\\\\" |> Parser.map (\() -> "\\")
                , Parser.succeed x0
                    |. Parser.symbol "\\X\\"
                    |= hexDigit
                    |= hexDigit
                , Parser.succeed x2
                    |. Parser.symbol "\\X2\\"
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,,,)
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                        )
                    |. Parser.symbol "\\X0\\"
                , Parser.succeed x4
                    |. Parser.symbol "\\X4\\"
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,,,,,)
                            |. Parser.symbol "0"
                            |. Parser.symbol "0"
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                        )
                    |. Parser.symbol "\\X0\\"
                , Parser.keep Parser.oneOrMore isBasic
                ]
            )


string : Decoder Attribute String
string =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.StringAttribute encodedString ->
                    case Parser.run decodeString encodedString of
                        Ok decodedString ->
                            Ok decodedString

                        Err err ->
                            Err
                                ("Could not parse encoded string '"
                                    ++ encodedString
                                )

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


referenced : Decoder Entity a -> Decoder Attribute a
referenced decoder =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.ReferenceTo entity ->
                    run decoder entity

                _ ->
                    Err "Expected a referenced entity"
        )


try : List (Decoder i a) -> List String -> i -> Result String a
try decoders errorMessages input =
    case decoders of
        [] ->
            -- No more decoders to try: fail with an error message that
            -- aggregates all the individual error messages
            Err
                ("All possible decoders failed (error messages: \""
                    ++ String.join "\", \"" (List.reverse errorMessages)
                    ++ "\")"
                )

        first :: rest ->
            -- At least one decoder left to try, so try it
            case run first input of
                -- Decoding succeeded: return the result
                Ok result ->
                    Ok result

                -- Decoding failed: move on to the next one, but save the error
                -- message in case *all* decoders fail (see above)
                Err message ->
                    try rest (message :: errorMessages) input


oneOf : List (Decoder i a) -> Decoder i a
oneOf decoders =
    Types.Decoder (try decoders [])


null : a -> Decoder Attribute a
null value =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.NullAttribute ->
                    Ok value

                _ ->
                    Err "Expecting null attribute ($)"
        )


default : a -> Decoder Attribute a
default value =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.DefaultAttribute ->
                    Ok value

                _ ->
                    Err "Expecting 'default value' attribute (*)"
        )


withDefault : a -> Decoder Attribute a -> Decoder Attribute a
withDefault value decoder =
    oneOf [ decoder, default value ]


optional : Decoder Attribute a -> Decoder Attribute (Maybe a)
optional decoder =
    oneOf [ map Just decoder, null Nothing ]


andThen : (a -> Decoder i b) -> Decoder i a -> Decoder i b
andThen function decoder =
    Types.Decoder
        (\input ->
            run decoder input
                |> Result.andThen
                    (\result ->
                        run (function result) input
                    )
        )


lazy : (() -> Decoder i a) -> Decoder i a
lazy constructor =
    Types.Decoder (\input -> run (constructor ()) input)
