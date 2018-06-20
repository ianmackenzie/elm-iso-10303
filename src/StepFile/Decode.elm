module StepFile.Decode
    exposing
        ( Decoder
        , andThen
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
import Parser exposing ((|.), (|=), Parser)
import StepFile.Entity as Entity exposing (Attribute, Entity)
import StepFile.Header as Header exposing (Header)
import StepFile.Types as Types exposing (StepFile)


{-| A `Decoder` describes how to attempt to decode a given `File`, `Entity` or
`Attribute` to produce a value of another type. See the `Decode` module for
details on how to use and construct decoders.
-}
type alias Decoder i a =
    Types.Decoder i a


run : Decoder i a -> i -> Result String a
run (Types.Decoder function) input =
    function input


succeed : a -> Decoder i a
succeed value =
    Types.Decoder (always (Ok value))


fail : String -> Decoder i a
fail description =
    Types.Decoder (always (Err description))


file : a -> Decoder StepFile a
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


header : Decoder StepFile Header
header =
    Types.Decoder (\(Types.StepFile file_) -> Ok file_.header)


filterEntities : (Entity -> Bool) -> StepFile -> List Entity
filterEntities predicate (Types.StepFile { entities }) =
    let
        accumulate id entity accumulated =
            if predicate entity then
                entity :: accumulated
            else
                accumulated
    in
    Dict.foldr accumulate [] entities


entityOfType : String -> Decoder Entity a -> Decoder StepFile a
entityOfType typeName_ entityDecoder =
    Types.Decoder
        (\file_ ->
            case filterEntities (Entity.hasType typeName_) file_ of
                [ singleEntity ] ->
                    case run entityDecoder singleEntity of
                        Ok value ->
                            Ok value

                        Err message ->
                            Err
                                ("In entity of type '"
                                    ++ typeName_
                                    ++ "': "
                                    ++ message
                                )

                _ ->
                    Err
                        ("Expecting a single entity of type '"
                            ++ typeName_
                            ++ "'"
                        )
        )


entityWhere : (Entity -> Bool) -> Decoder Entity a -> Decoder StepFile a
entityWhere predicate entityDecoder =
    Types.Decoder
        (\file_ ->
            case filterEntities predicate file_ of
                [ singleEntity ] ->
                    run entityDecoder singleEntity

                _ ->
                    Err "Expecting a single matching entity"
        )


entitiesOfType : String -> Decoder Entity a -> Decoder StepFile (List a)
entitiesOfType typeName_ entityDecoder =
    entitiesWhere (Entity.hasType typeName_) entityDecoder


entitiesWhere : (Entity -> Bool) -> Decoder Entity a -> Decoder StepFile (List a)
entitiesWhere predicate entityDecoder =
    Types.Decoder
        (\file_ ->
            let
                filteredEntities =
                    filterEntities predicate file_
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


entitiesBy : (Entity -> Maybe (Decoder Entity a)) -> Decoder StepFile (List a)
entitiesBy decoderForEntity =
    Types.Decoder
        (\(Types.StepFile { entities }) ->
            decodeAllBy decoderForEntity (Dict.values entities) []
        )


attribute : Int -> Decoder Attribute a -> Decoder Entity a
attribute index attributeDecoder =
    Types.Decoder
        (\entity ->
            case List.getAt index (Entity.attributes entity) of
                Just attribute_ ->
                    case run attributeDecoder attribute_ of
                        Ok value ->
                            Ok value

                        Err message ->
                            Err
                                ("At attribute index "
                                    ++ String.fromInt index
                                    ++ ": "
                                    ++ message
                                )

                Nothing ->
                    Err ("No attribute at index " ++ String.fromInt index)
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
        (\attribute_ ->
            case attribute_ of
                Types.BoolAttribute value ->
                    Ok value

                _ ->
                    Err "Expected a bool"
        )


int : Decoder Attribute Int
int =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
                Types.IntAttribute value ->
                    Ok value

                _ ->
                    Err "Expected an int"
        )


float : Decoder Attribute Float
float =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
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
        [ Parser.succeed 0 |. Parser.token "0"
        , Parser.succeed 1 |. Parser.token "1"
        , Parser.succeed 2 |. Parser.token "2"
        , Parser.succeed 3 |. Parser.token "3"
        , Parser.succeed 4 |. Parser.token "4"
        , Parser.succeed 5 |. Parser.token "5"
        , Parser.succeed 6 |. Parser.token "6"
        , Parser.succeed 7 |. Parser.token "7"
        , Parser.succeed 8 |. Parser.token "8"
        , Parser.succeed 9 |. Parser.token "9"
        , Parser.succeed 10 |. Parser.token "A"
        , Parser.succeed 11 |. Parser.token "B"
        , Parser.succeed 12 |. Parser.token "C"
        , Parser.succeed 13 |. Parser.token "D"
        , Parser.succeed 14 |. Parser.token "E"
        , Parser.succeed 15 |. Parser.token "F"
        ]


x0 : Int -> Int -> Char
x0 high low =
    Char.fromCode (low + Bitwise.shiftLeftBy 4 high)


x2 : Int -> Int -> Int -> Int -> Char
x2 a b c d =
    Char.fromCode <|
        d
            + Bitwise.shiftLeftBy 4 c
            + Bitwise.shiftLeftBy 8 b
            + Bitwise.shiftLeftBy 12 a


x4 : Int -> Int -> Int -> Int -> Int -> Int -> Char
x4 a b c d e f =
    Char.fromCode <|
        f
            + Bitwise.shiftLeftBy 4 e
            + Bitwise.shiftLeftBy 8 d
            + Bitwise.shiftLeftBy 12 c
            + Bitwise.shiftLeftBy 16 b
            + Bitwise.shiftLeftBy 20 a


parseX0 : Parser String
parseX0 =
    Parser.succeed (\a b -> String.fromChar (x0 a b))
        |. Parser.token "\\X\\"
        |= hexDigit
        |= hexDigit


parseX2 : Parser String
parseX2 =
    Parser.succeed String.fromList
        |. Parser.token "\\X2\\"
        |= Parser.loop []
            (\accumulated ->
                Parser.oneOf
                    [ Parser.succeed
                        (\a b c d ->
                            Parser.Loop (x2 a b c d :: accumulated)
                        )
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                    , Parser.succeed
                        (\() -> Parser.Done (List.reverse accumulated))
                        |= Parser.token "\\X0\\"
                    ]
            )


parseX4 : Parser String
parseX4 =
    Parser.succeed String.fromList
        |. Parser.token "\\X4\\"
        |= Parser.loop []
            (\accumulated ->
                Parser.oneOf
                    [ Parser.succeed
                        (\a b c d e f ->
                            Parser.Loop (x4 a b c d e f :: accumulated)
                        )
                        |. Parser.token "00"
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                        |= hexDigit
                    , Parser.succeed
                        (\() -> Parser.Done (List.reverse accumulated))
                        |= Parser.token "\\X0\\"
                    ]
            )


parseStringChunk : Parser String
parseStringChunk =
    Parser.oneOf
        [ Parser.getChompedString
            (Parser.chompIf isBasic |. Parser.chompWhile isBasic)
        , Parser.succeed "'" |. Parser.token "''"
        , Parser.succeed "\\" |. Parser.token "\\\\"
        , parseX0
        , parseX2
        , parseX4
        ]


parseString : Parser String
parseString =
    Parser.succeed String.concat
        |= Parser.loop []
            (\accumulated ->
                Parser.oneOf
                    [ Parser.succeed
                        (\chunk -> Parser.Loop (chunk :: accumulated))
                        |= parseStringChunk
                    , Parser.lazy
                        (\() ->
                            Parser.succeed <|
                                Parser.Done (List.reverse accumulated)
                        )
                    ]
            )


string : Decoder Attribute String
string =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
                Types.StringAttribute encodedString ->
                    case Parser.run parseString encodedString of
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
collectDecodedAttributes decoder collected attributes_ =
    case attributes_ of
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
        (\attribute_ ->
            case attribute_ of
                Types.AttributeList attributes_ ->
                    collectDecodedAttributes itemDecoder [] attributes_

                _ ->
                    Err "Expected a list"
        )


tuple2 : ( Decoder Attribute a, Decoder Attribute b ) -> Decoder Attribute ( a, b )
tuple2 ( firstDecoder, secondDecoder ) =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
                Types.AttributeList [ firstAttribute, secondAttribute ] ->
                    Result.map2 Tuple.pair
                        (run firstDecoder firstAttribute)
                        (run secondDecoder secondAttribute)

                _ ->
                    Err "Expected a list of two items"
        )


tuple3 : ( Decoder Attribute a, Decoder Attribute b, Decoder Attribute c ) -> Decoder Attribute ( a, b, c )
tuple3 ( firstDecoder, secondDecoder, thirdDecoder ) =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
                Types.AttributeList [ firstAttribute, secondAttribute, thirdAttribute ] ->
                    Result.map3
                        (\first second third -> ( first, second, third ))
                        (run firstDecoder firstAttribute)
                        (run secondDecoder secondAttribute)
                        (run thirdDecoder thirdAttribute)

                _ ->
                    Err "Expected a list of three items"
        )


referencedEntity : Decoder Entity a -> Decoder Attribute a
referencedEntity entityDecoder =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
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
        (\attribute_ ->
            case attribute_ of
                Types.NullAttribute ->
                    Ok value

                _ ->
                    Err "Expecting null attribute ($)"
        )


default : a -> Decoder Attribute a
default value =
    Types.Decoder
        (\attribute_ ->
            case attribute_ of
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
