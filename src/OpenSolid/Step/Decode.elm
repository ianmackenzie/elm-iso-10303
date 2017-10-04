module OpenSolid.Step.Decode
    exposing
        ( andThen
        , attribute
        , attributes
        , bool
        , default
        , entitiesBy
        , entitiesOfType
        , entitiesWhere
        , entityOfType
        , entityWhere
        , fail
        , file
        , float
        , int
        , lazy
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , null
        , oneOf
        , optional
        , referencedEntity
        , run
        , string
        , succeed
        , toAttribute
        , toEntity
        , tuple2
        , tuple3
        , typeName
        , withDefault
        )

import Bitwise
import Dict
import List
import List.Extra as List
import OpenSolid.Step exposing (Attribute, Decoder, Entity, File, Header)
import OpenSolid.Step.Entity as Entity
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


decodeAll : Decoder i a -> List i -> List a -> Result String (List a)
decodeAll decoder inputs accumulated =
    case inputs of
        [] ->
            Ok (List.reverse accumulated)

        first :: rest ->
            case run decoder first of
                Ok value ->
                    decodeAll decoder rest (value :: accumulated)

                Err message ->
                    Err message


header : Decoder File Header
header =
    Types.Decoder (\(Types.File { header }) -> Ok header)


filterEntities : (Entity -> Bool) -> File -> List Entity
filterEntities predicate (Types.File { entities }) =
    let
        accumulate id entity accumulated =
            if predicate entity then
                entity :: accumulated
            else
                accumulated
    in
    Dict.foldr accumulate [] entities


entityOfType : String -> Decoder Entity a -> Decoder File a
entityOfType typeName entityDecoder =
    Types.Decoder
        (\file ->
            case filterEntities (Entity.hasType typeName) file of
                [ singleEntity ] ->
                    case run entityDecoder singleEntity of
                        Ok value ->
                            Ok value

                        Err message ->
                            Err
                                ("In entity of type '"
                                    ++ typeName
                                    ++ "': "
                                    ++ message
                                )

                _ ->
                    Err
                        ("Expecting a single entity of type '"
                            ++ typeName
                            ++ "'"
                        )
        )


entityWhere : (Entity -> Bool) -> Decoder Entity a -> Decoder File a
entityWhere predicate entityDecoder =
    Types.Decoder
        (\file ->
            case filterEntities predicate file of
                [ singleEntity ] ->
                    run entityDecoder singleEntity

                _ ->
                    Err "Expecting a single matching entity"
        )


entitiesOfType : String -> Decoder Entity a -> Decoder File (List a)
entitiesOfType typeName entityDecoder =
    Types.Decoder
        (\file ->
            let
                filteredEntities =
                    filterEntities (Entity.hasType typeName) file
            in
            decodeAll entityDecoder filteredEntities []
        )


entitiesWhere : (Entity -> Bool) -> Decoder Entity a -> Decoder File (List a)
entitiesWhere predicate entityDecoder =
    Types.Decoder
        (\file ->
            let
                filteredEntities =
                    filterEntities predicate file
            in
            decodeAll entityDecoder filteredEntities []
        )


decodeAllBy : (i -> Maybe (Decoder i a)) -> List i -> List a -> Result String (List a)
decodeAllBy decoderForInput inputs accumulated =
    case inputs of
        [] ->
            Ok (List.reverse accumulated)

        first :: rest ->
            case decoderForInput first of
                Just decoder ->
                    case run decoder first of
                        Ok value ->
                            decodeAllBy decoderForInput
                                rest
                                (value :: accumulated)

                        Err message ->
                            Err message

                Nothing ->
                    decodeAllBy decoderForInput rest accumulated


entitiesBy : (Entity -> Maybe (Decoder Entity a)) -> Decoder File (List a)
entitiesBy decoderForEntity =
    Types.Decoder
        (\(Types.File { entities }) ->
            decodeAllBy decoderForEntity (Dict.values entities) []
        )


attribute : Int -> Decoder Attribute a -> Decoder Entity a
attribute index attributeDecoder =
    Types.Decoder
        (\entity ->
            case List.getAt index (Entity.attributes entity) of
                Just attribute ->
                    case run attributeDecoder attribute of
                        Ok value ->
                            Ok value

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


map2 :
    (a -> b -> c)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
map2 function (Types.Decoder functionA) (Types.Decoder functionB) =
    Types.Decoder
        (\input ->
            Result.map2 function
                (functionA input)
                (functionB input)
        )


map3 :
    (a -> b -> c -> d)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
map3 function (Types.Decoder functionA) (Types.Decoder functionB) (Types.Decoder functionC) =
    Types.Decoder
        (\input ->
            Result.map3 function
                (functionA input)
                (functionB input)
                (functionC input)
        )


map4 :
    (a -> b -> c -> d -> e)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
map4 function (Types.Decoder functionA) (Types.Decoder functionB) (Types.Decoder functionC) (Types.Decoder functionD) =
    Types.Decoder
        (\input ->
            Result.map4 function
                (functionA input)
                (functionB input)
                (functionC input)
                (functionD input)
        )


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
    -> Decoder i f
map5 function (Types.Decoder functionA) (Types.Decoder functionB) (Types.Decoder functionC) (Types.Decoder functionD) (Types.Decoder functionE) =
    Types.Decoder
        (\input ->
            Result.map5 function
                (functionA input)
                (functionB input)
                (functionC input)
                (functionD input)
                (functionE input)
        )


map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
    -> Decoder i f
    -> Decoder i g
map6 function decoderA decoderB decoderC decoderD decoderE decoderF =
    decoderA
        |> andThen
            (\valueA ->
                map5 (function valueA)
                    decoderB
                    decoderC
                    decoderD
                    decoderE
                    decoderF
            )


map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
    -> Decoder i f
    -> Decoder i g
    -> Decoder i h
map7 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    decoderA
        |> andThen
            (\valueA ->
                map6 (function valueA)
                    decoderB
                    decoderC
                    decoderD
                    decoderE
                    decoderF
                    decoderG
            )


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> j)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
    -> Decoder i f
    -> Decoder i g
    -> Decoder i h
    -> Decoder i j
map8 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    decoderA
        |> andThen
            (\valueA ->
                map7 (function valueA)
                    decoderB
                    decoderC
                    decoderD
                    decoderE
                    decoderF
                    decoderG
                    decoderH
            )


mapError : (String -> String) -> Decoder i a -> Decoder i a
mapError mapFunction (Types.Decoder function) =
    Types.Decoder (function >> Result.mapError mapFunction)


toEntity : Decoder Entity Entity
toEntity =
    Types.Decoder Ok


typeName : Decoder Entity String
typeName =
    Types.Decoder (Ok << Entity.typeName)


attributes : Decoder Entity (List Attribute)
attributes =
    Types.Decoder (Ok << Entity.attributes)


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


referencedEntity : Decoder Entity a -> Decoder Attribute a
referencedEntity entityDecoder =
    Types.Decoder
        (\attribute ->
            case attribute of
                Types.ReferenceTo entity ->
                    run entityDecoder entity

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
