module Step.Decode exposing
    ( Decoder, File, Error(..)
    , file, header, single, all
    , singleTopLevel, allTopLevel
    , entity, entityId
    , attribute
    , bool, int, float, string, enum, referenceTo, binaryData, null, optional, list, tuple2, tuple3, derivedValue
    , boolAs, intAs, floatAs, stringAs, enumAs, binaryDataAs, listAs
    , succeed, fail, map, map2, map3, map4, map5, map6, map7, map8, resolve, andThen, flatten, oneOf, lazy, identity
    )

{-| This module lets you decode data from STEP files in a similar way to how
you decode [JSON](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode).


# Decoder types

@docs Decoder, File, Error


# Decoding a file

@docs file, header, single, all


## Top-level entities

It may be useful in some cases to only decode _top-level_ entities (entities
that are not referenced by any other entities). These otherwise work just like
`single` and `all`.

@docs singleTopLevel, allTopLevel


# Decoding an entity

@docs entity, entityId


# Decoding attributes

@docs attribute

@docs bool, int, float, string, enum, referenceTo, binaryData, null, optional, list, tuple2, tuple3, derivedValue


## Typed attributes

@docs boolAs, intAs, floatAs, stringAs, enumAs, binaryDataAs, listAs


# Working with decoders

@docs succeed, fail, map, map2, map3, map4, map5, map6, map7, map8, resolve, andThen, flatten, oneOf, lazy, identity

-}

import Bitwise
import Bytes.Decode
import Dict
import List
import List.Extra as List
import Parser exposing ((|.), (|=), Parser)
import Step.EntityResolution as EntityResolution
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.FastParse as FastParse
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types as Types exposing (Attribute, Entity, Header, SubEntity)


{-| A `Decoder` describes how to attempt to decode some input of type `i` (an
entire file, an individual entity, a specific attribute) and produce some output
of type `a`.
-}
type Decoder i a
    = Decoder (i -> DecodeResult a)


{-| Represents an entire STEP file composed of a header and a list of entities.
The only way to extract data from a `File` is by using a decoder, so you'll
likely never need to refer to or use this type directly.
-}
type File
    = File
        { header : Header
        , allEntities : List Entity
        , topLevelEntities : List Entity
        }


{-| Different kinds of errors that may be encountered when loading a STEP file:

  - `ParseError`: the file could not be parsed as a valid STEP file.
  - `NonexistentEntity`: one entity in the STEP file refers to another by
    integer ID, but no entity with that ID exists in the file.
  - `CircularReference`: There exists a circular chain of entities in the file
    that refer to each other; the `List Int` is the list of entity IDs in the
    chain.
  - `DecodeError`: the file was parsed OK and all entity references were
    resolved successfully, but an error occurred when running the given decoder.

-}
type Error
    = ParseError String
    | NonexistentEntity Int
    | CircularReference (List Int)
    | DecodeError String


type DecodeResult a
    = Succeeded a
    | Failed String
    | UnexpectedType


decodeResult : Decoder i a -> i -> DecodeResult a
decodeResult (Decoder function) input =
    function input


{-| Run a decoder on some input. You will not usually need to use this function
directly, but it may be necessary if you use [`Decode.identity`](#identity) to
access raw [`Entity`](Step-Types#Entity) or [`Attribute`](Step-Types#Attribute)
values.
-}
run : Decoder i a -> i -> Result String a
run decoder input =
    case decodeResult decoder input of
        Succeeded value ->
            Ok value

        Failed message ->
            Err message

        UnexpectedType ->
            Err "Unexpected type"


{-| A special decoder that always succeeds with the given value. Primarily
useful with [`andThen`](#andThen).
-}
succeed : a -> Decoder i a
succeed value =
    Decoder (always (Succeeded value))


{-| A special decoder that always fails with the given error message.
-}
fail : String -> Decoder i a
fail description =
    Decoder (always (Failed description))


{-| A special decoder that just returns its input unmodified. This can be useful
if you want to get a raw [`Entity`](Step-Types#Entity) or [`Attribute`](Step-Types#Attribute)
value and inspect it.
-}
identity : Decoder i i
identity =
    Decoder Succeeded


{-| Decode a STEP file given as a `String` using the given decoder. For example,
to extract the file header and all `CARTESIAN_POINT` entities from a given file,
you might write:

    import Step.Types as Step
    import Step.Decode as Decode exposing (Decoder)

    type alias FileData =
        { header : Step.Header
        , points : List ( Float, Float, Float )
        }

    pointDecoder : Decoder Step.Entity ( Float, Float, Float )
    pointDecoder =
        Decode.entity "CARTESIAN_POINT" <|
            Decode.attribute 1 (Decode.tuple3 Decode.float)

    fileDecoder : Decoder Step.File FileData
    fileDecoder =
        Decode.map2 FileData
            Decode.header
            (Decode.all pointDecoder)

    fileContents : String
    fileContents =
        "ISO-10303-21;...END-ISO-10303-21;"

    decoded : Result Decode.Error FileData
    decoded =
        Decode.file fileDecoder fileContents

-}
file : Decoder File a -> String -> Result Error a
file decoder contents =
    FastParse.parse contents
        |> Result.mapError ParseError
        |> Result.andThen
            (\parsed ->
                case EntityResolution.resolve parsed.entities of
                    Ok resolved ->
                        Ok <|
                            File
                                { header = parsed.header
                                , allEntities = resolved.allEntities
                                , topLevelEntities = resolved.topLevelEntities
                                }

                    Err (EntityResolution.NonexistentEntity id) ->
                        Err (NonexistentEntity id)

                    Err (EntityResolution.CircularReference chain) ->
                        Err (CircularReference chain)
            )
        |> Result.andThen
            (\inputFile ->
                case decodeResult decoder inputFile of
                    Succeeded value ->
                        Ok value

                    Failed message ->
                        Err (DecodeError message)

                    UnexpectedType ->
                        Err (DecodeError "Internal error: UnexpectedType from file decoder")
            )


{-| Extract the [header](Step-File#Header) of a STEP file.
-}
header : Decoder File Header
header =
    Decoder
        (\(File properties) ->
            Succeeded properties.header
        )


{-| Attempt to find exactly one entity in a file that matches the given decoder.
If there are no matching entities or more than one matching entity, the decoder
will fail.
-}
single : Decoder Entity a -> Decoder File a
single entityDecoder =
    Decoder
        (\(File properties) ->
            singleEntity entityDecoder properties.allEntities Nothing
        )


{-| Attempt to find exactly one top-level entity in a file that matches the
given decoder.
-}
singleTopLevel : Decoder Entity a -> Decoder File a
singleTopLevel entityDecoder =
    Decoder
        (\(File properties) ->
            singleEntity entityDecoder properties.topLevelEntities Nothing
        )


singleEntity : Decoder Entity a -> List Entity -> Maybe a -> DecodeResult a
singleEntity decoder entities currentValue =
    case entities of
        [] ->
            case currentValue of
                Just value ->
                    Succeeded value

                Nothing ->
                    Failed "No matching entities found"

        first :: rest ->
            case decodeResult decoder first of
                Succeeded value ->
                    case currentValue of
                        Nothing ->
                            singleEntity decoder rest (Just value)

                        Just _ ->
                            Failed "More than one matching entity found"

                Failed message ->
                    Failed message

                UnexpectedType ->
                    singleEntity decoder rest currentValue


{-| Find all entities in a file matching the given decoder.
-}
all : Decoder Entity a -> Decoder File (List a)
all entityDecoder =
    Decoder
        (\(File properties) ->
            allEntities entityDecoder properties.allEntities []
        )


{-| Find all top-level entities in a file matching the given decoder.
-}
allTopLevel : Decoder Entity a -> Decoder File (List a)
allTopLevel entityDecoder =
    Decoder
        (\(File properties) ->
            allEntities entityDecoder properties.topLevelEntities []
        )


allEntities : Decoder Entity a -> List Entity -> List a -> DecodeResult (List a)
allEntities decoder entities accumulated =
    case entities of
        [] ->
            Succeeded (List.reverse accumulated)

        first :: rest ->
            case decodeResult decoder first of
                Succeeded value ->
                    allEntities decoder rest (value :: accumulated)

                Failed message ->
                    Failed message

                UnexpectedType ->
                    allEntities decoder rest accumulated


annotateWithEntityId : Maybe Int -> String -> String
annotateWithEntityId maybeId message =
    case maybeId of
        Just id ->
            "In entity " ++ String.fromInt id ++ ": " ++ message

        Nothing ->
            message


{-| Decode an entity of the given type name, using the given decoder on the
entity's list of attributes.

The decoder will skip any entities with a different type name. This means that,
for example,

    Decode.all <|
        Decode.entity "SOME_TYPE" <|
            Decode.attribute 0 attributeDecoder

will attempt to decode all entities of type `SOME_TYPE` and skip the rest.

-}
entity : String -> Decoder (List Attribute) a -> Decoder Entity a
entity givenTypeName decoder =
    let
        searchTypeName =
            TypeName.fromString givenTypeName
    in
    Decoder
        (\currentEntity ->
            case currentEntity of
                Types.SimpleEntity currentId typeName attributes ->
                    if typeName == searchTypeName then
                        case decodeResult decoder attributes of
                            Succeeded a ->
                                Succeeded a

                            Failed message ->
                                Failed (annotateWithEntityId currentId message)

                            UnexpectedType ->
                                Failed "Internal error: UnexpectedType from attribute list decoder"

                    else
                        UnexpectedType

                Types.ComplexEntity currentId subEntities ->
                    partialEntity searchTypeName decoder currentId subEntities
        )


partialEntity : TypeName -> Decoder (List Attribute) a -> Maybe Int -> List SubEntity -> DecodeResult a
partialEntity searchTypeName decoder currentId subEntities =
    case subEntities of
        [] ->
            UnexpectedType

        (Types.SubEntity typeName attributes) :: rest ->
            if typeName == searchTypeName then
                case decodeResult decoder attributes of
                    Succeeded value ->
                        Succeeded value

                    Failed message ->
                        Failed (annotateWithEntityId currentId message)

                    UnexpectedType ->
                        Failed "Internal error: UnexpectedType from attribute list decoder"

            else
                partialEntity searchTypeName decoder currentId rest


getEntityId : Entity -> Maybe Int
getEntityId givenEntity =
    case givenEntity of
        Types.SimpleEntity maybeId _ _ ->
            maybeId

        Types.ComplexEntity maybeId _ ->
            maybeId


{-| Get the integer ID of a particular entity in a file. Note that these are not
guaranteed to be consistent between two different versions of a given STEP file.
-}
entityId : Decoder Entity Int
entityId =
    Decoder
        (\currentEntity ->
            case getEntityId currentEntity of
                Just currentId ->
                    Succeeded currentId

                Nothing ->
                    Failed "Entity has no ID (was it created manually instead of read from a STEP file?)"
        )


{-| Decode a specific attribute by index (starting from 0) in an attribute list.
To decode data from multiple attributes in a list, you can use one of the `mapN`
functions, for example:

    type alias MyAttributes =
        { name : String
        , age : Int
        , height : Float
        }

    myAttributesDecoder : Decoder (List Attribute) MyAttributes
    myAttributesDecoder =
        Decode.map3 MyAttributes
            (Decode.attribute 0 Decode.string)
            (Decode.attribute 1 Decode.int)
            (Decode.attribute 2 Decode.float)

-}
attribute : Int -> Decoder Attribute a -> Decoder (List Attribute) a
attribute index attributeDecoder =
    Decoder
        (\attributeList ->
            case List.getAt index attributeList of
                Just entityAttribute ->
                    case decodeResult attributeDecoder entityAttribute of
                        Succeeded value ->
                            Succeeded value

                        Failed message ->
                            Failed ("At attribute index " ++ String.fromInt index ++ ": " ++ message)

                        UnexpectedType ->
                            Failed "Internal error: UnexpectedType from attribute decoder"

                Nothing ->
                    Failed ("No attribute at index " ++ String.fromInt index)
        )


mapHelp : (a -> b) -> DecodeResult a -> DecodeResult b
mapHelp function result =
    case result of
        Succeeded value ->
            Succeeded (function value)

        Failed message ->
            Failed message

        UnexpectedType ->
            UnexpectedType


{-| Map the value produced by a decoder.
-}
map : (a -> b) -> Decoder i a -> Decoder i b
map mapFunction decoder =
    Decoder (\input -> mapHelp mapFunction (decodeResult decoder input))


map2Help : (a -> b -> c) -> DecodeResult a -> DecodeResult b -> DecodeResult c
map2Help function resultA resultB =
    case resultA of
        Succeeded value ->
            mapHelp (function value) resultB

        UnexpectedType ->
            UnexpectedType

        Failed message ->
            Failed message


{-| Map over two decoders.
-}
map2 :
    (a -> b -> c)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
map2 function (Decoder functionA) (Decoder functionB) =
    Decoder (\input -> map2Help function (functionA input) (functionB input))


{-| Map over three decoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
map3 function decoderA decoderB decoderC =
    map2 (<|) (map2 function decoderA decoderB) decoderC


{-| Map over four decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
map4 function decoderA decoderB decoderC decoderD =
    map2 (<|) (map3 function decoderA decoderB decoderC) decoderD


{-| Map over five decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder i a
    -> Decoder i b
    -> Decoder i c
    -> Decoder i d
    -> Decoder i e
    -> Decoder i f
map5 function decoderA decoderB decoderC decoderD decoderE =
    map2 (<|) (map4 function decoderA decoderB decoderC decoderD) decoderE


{-| Map over six decoders.
-}
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
    map2 (<|) (map5 function decoderA decoderB decoderC decoderD decoderE) decoderF


{-| Map over seven decoders.
-}
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
    map2 (<|) (map6 function decoderA decoderB decoderC decoderD decoderE decoderF) decoderG


{-| Map over eight decoders.
-}
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
    map2 (<|) (map7 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG) decoderH


{-| It is often useful to call functions like `Decode.map2` but pass a mapping
function that may fail (a function that returns a `Result String a` instead of
just a value of type `a`). For example, you might try to decode the ratio of two
numbers, failing if the denominator is zero:

    Decode.map2
        (\numerator denominator ->
            if denominator == 0 then
                Err "Denominator cannot be zero"

            else
                Ok (numerator / denominator)
        )
        (Decode.attribute 0 Decode.float)
        (Decode.attribute 1 Decode.float)

However, this is now a `Decoder (List Attribute) (Result String Float)` instead
of just a `Decoder (List Attribute) Float`. Using `resolve` will convert the
`Ok` case to a successful decode and the `Err` case to a decode error, giving
you the decoder that you want:

    ratioDecoder : Decoder (List Attribute) Float
    ratioDecoder =
        Decode.resolve <|
            Decode.map2
                (\numerator denominator ->
                    if denominator == 0 then
                        Err "Denominator cannot be zero"

                    else
                        Ok (numerator / denominator)
                )
                (Decode.attribute 0 Decode.float)
                (Decode.attribute 1 Decode.float)

-}
resolve : Decoder i (Result String a) -> Decoder i a
resolve decoder =
    decoder
        |> andThen
            (\result ->
                case result of
                    Ok value ->
                        succeed value

                    Err message ->
                        fail message
            )


{-| Decode a single attribute as a `Bool` (from the special STEP enum values
`.T.` and `.F.`).
-}
bool : Decoder Attribute Bool
bool =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.BoolAttribute value ->
                    Succeeded value

                _ ->
                    Failed "Expected a bool"
        )


{-| Decode a single attribute as an `Int`.
-}
int : Decoder Attribute Int
int =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.IntAttribute value ->
                    Succeeded value

                _ ->
                    Failed "Expected an int"
        )


{-| Decode a single attribute as a `Float`. Note that unlike JSON, STEP has
different encodings for `Int` and `Float` values so you cannot use this
function to decode integer values; if you want to decode an integer value as a
`Float` you will need to use

    Decode.map toFloat Decode.int

-}
float : Decoder Attribute Float
float =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.FloatAttribute value ->
                    Succeeded value

                Types.NullAttribute ->
                    -- Some STEP files seem to use $ to indicate NaN even though
                    -- that doesn't seem to be allowed by the spec
                    Succeeded (0 / 0)

                _ ->
                    Failed "Expected a float"
        )


isBasic : Char -> Bool
isBasic character =
    character /= '\'' && character /= '\\'


{-| Decode a single attribute as a `String`.
-}
string : Decoder Attribute String
string =
    Decoder
        (\attribute_ ->
            case attribute_ of
                Types.StringAttribute value ->
                    Succeeded value

                _ ->
                    Failed "Expected a string"
        )


{-| Decode a single attribute as a blob of binary data, using the given
[binary decoder](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode).
-}
binaryData : Bytes.Decode.Decoder a -> Decoder Attribute a
binaryData bytesDecoder =
    Decoder
        (\attribute_ ->
            case attribute_ of
                Types.BinaryDataAttribute value ->
                    case Bytes.Decode.decode bytesDecoder value of
                        Just decodedValue ->
                            Succeeded decodedValue

                        Nothing ->
                            Failed "Could not parse binary data"

                _ ->
                    Failed "Expected binary data"
        )


{-| Decode a single enum attribute, by passing a list of enum cases as their
STEP type name and corresponding Elm value. For example, given a STEP enum with
values `RED`, `YELLOW` and `GREEN` you might write:

    type LightColor
        = Red
        | Yellow
        | Green

    lightColorDecoder : Decoder Attribute LightColor
    lightColorDecoder =
        Decode.enum
            [ ( "RED", Red )
            , ( "YELLOW", Yellow )
            , ( "GREEN", Green )
            ]

(Note that the given strings will be normalized, so case does not matter.)

-}
enum : List ( String, a ) -> Decoder Attribute a
enum cases =
    let
        lookupDict =
            cases
                |> List.map (Tuple.mapFirst (EnumValue.fromString >> EnumValue.toString))
                |> Dict.fromList
    in
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.EnumAttribute enumValue ->
                    case Dict.get (EnumValue.toString enumValue) lookupDict of
                        Just value ->
                            Succeeded value

                        Nothing ->
                            Failed
                                ("Unrecognized enum value '"
                                    ++ EnumValue.toString enumValue
                                    ++ "'"
                                )

                _ ->
                    Failed "Expected an enum"
        )


collectDecodedAttributes : Decoder Attribute a -> List a -> List Attribute -> DecodeResult (List a)
collectDecodedAttributes decoder accumulated remainingAttributes =
    case remainingAttributes of
        [] ->
            -- No more attributes to decode, so succeed with all the results we
            -- have collected so far
            Succeeded (List.reverse accumulated)

        first :: rest ->
            case decodeResult decoder first of
                -- Decoding succeeded on this attribute: continue with the
                -- rest
                Succeeded result ->
                    collectDecodedAttributes decoder (result :: accumulated) rest

                -- Decoding failed on this attribute: immediately abort
                -- with the returned error message
                Failed message ->
                    Failed message

                UnexpectedType ->
                    Failed "Internal error: UnexpectedType from attribute decoder"


{-| Decode an attribute as a list, passing the decoder to be used for each list
item.
-}
list : Decoder Attribute a -> Decoder Attribute (List a)
list itemDecoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.AttributeList attributes_ ->
                    collectDecodedAttributes itemDecoder [] attributes_

                _ ->
                    Failed "Expected a list"
        )


typedAttribute : String -> Decoder Attribute a -> Decoder Attribute a
typedAttribute givenTypeName decoder =
    let
        expectedTypeName =
            TypeName.fromString givenTypeName
    in
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.TypedAttribute attributeTypeName underlyingAttribute ->
                    if attributeTypeName == expectedTypeName then
                        decodeResult decoder underlyingAttribute

                    else
                        Failed
                            ("Expected a typed attribute of type '"
                                ++ TypeName.toString expectedTypeName
                                ++ "', got '"
                                ++ TypeName.toString attributeTypeName
                                ++ "'"
                            )

                _ ->
                    Failed "Expected a typed attribute"
        )


{-| Decode a `Bool` wrapped as the given type.
-}
boolAs : String -> Decoder Attribute Bool
boolAs givenTypeName =
    typedAttribute givenTypeName bool


{-| Decode an `Int` wrapped as the given type.
-}
intAs : String -> Decoder Attribute Int
intAs givenTypeName =
    typedAttribute givenTypeName int


{-| Decode a `Float` wrapped as the given type.
-}
floatAs : String -> Decoder Attribute Float
floatAs givenTypeName =
    typedAttribute givenTypeName float


{-| Decode a `String` wrapped as the given type.
-}
stringAs : String -> Decoder Attribute String
stringAs givenTypeName =
    typedAttribute givenTypeName string


{-| Decode binary data wrapped as the given type.

Note that while the STEP format supports binary data with an arbitrary number of
bits, Elm only supports byte-aligned binary data (where the number of bits is a
multiple of eight). This means that the `Bytes` value passed to the decoder may
be zero-padded. For example, if a STEP file contained a binary data attribute
with the value `011001` (six bits), the Elm `Bytes` value that would actually
end up being decoded would be `00011001` (eight bits/one byte).

-}
binaryDataAs : String -> Bytes.Decode.Decoder a -> Decoder Attribute a
binaryDataAs givenTypeName bytesDecoder =
    typedAttribute givenTypeName (binaryData bytesDecoder)


{-| Decode an enum wrapped as the given type.
-}
enumAs : String -> List ( String, a ) -> Decoder Attribute a
enumAs givenTypeName cases =
    typedAttribute givenTypeName (enum cases)


{-| Decode a list wrapped as the given type.
-}
listAs : String -> Decoder Attribute a -> Decoder Attribute (List a)
listAs givenTypeName itemDecoder =
    typedAttribute givenTypeName (list itemDecoder)


{-| Decode a list of exactly two elements, passing the decoder to be used for
the two elements.
-}
tuple2 : Decoder Attribute a -> Decoder Attribute ( a, a )
tuple2 decoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.AttributeList [ firstAttribute, secondAttribute ] ->
                    map2Help Tuple.pair
                        (decodeResult decoder firstAttribute)
                        (decodeResult decoder secondAttribute)

                _ ->
                    Failed "Expected a list of two items"
        )


{-| Decode a list of exactly three elements, passing the decoder to be used for
the three elements.
-}
tuple3 : Decoder Attribute a -> Decoder Attribute ( a, a, a )
tuple3 decoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.AttributeList [ firstAttribute, secondAttribute, thirdAttribute ] ->
                    map2Help (<|)
                        (map2Help (\first second third -> ( first, second, third ))
                            (decodeResult decoder firstAttribute)
                            (decodeResult decoder secondAttribute)
                        )
                        (decodeResult decoder thirdAttribute)

                _ ->
                    Failed "Expected a list of three items"
        )


{-| Decode an attribute which is a reference to another entity, by providing the
decoder to use for that entity.
-}
referenceTo : Decoder Entity a -> Decoder Attribute a
referenceTo entityDecoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.ReferenceTo referencedEntity ->
                    case decodeResult entityDecoder referencedEntity of
                        Succeeded value ->
                            Succeeded value

                        Failed message ->
                            Failed message

                        UnexpectedType ->
                            Failed (annotateWithEntityId (getEntityId referencedEntity) "Unexpected type")

                _ ->
                    Failed "Expected a referenced entity"
        )


{-| Construct an entity decoder that may match one of many entity types. The
first decoder with a matching type will be used. If none of the entity types
match, then the current entity is skipped.
-}
oneOf : List (Decoder Entity a) -> Decoder Entity a
oneOf decoders =
    Decoder (oneOfHelp decoders)


oneOfHelp : List (Decoder Entity a) -> Entity -> DecodeResult a
oneOfHelp decoders input =
    case decoders of
        first :: rest ->
            -- At least one decoder left to try, so try it
            case decodeResult first input of
                -- Decoding succeeded: return the result
                Succeeded result ->
                    Succeeded result

                -- Decoder matched the entity (based on type name) but then
                -- failed decoding; in this case consider that the decoder has
                -- 'committed' to decoding and so report the error instead of
                -- ignoring it and moving to the next decoder
                Failed message ->
                    Failed message

                -- Decoder did not match the entity: move on to the next one,
                -- but save the error message in case *all* decoders do not
                -- match (see above)
                UnexpectedType ->
                    oneOfHelp rest input

        [] ->
            -- No more decoders to try
            UnexpectedType


{-| Decode the special 'null' attribute (`$`) as the given value.
-}
null : a -> Decoder Attribute a
null value =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.NullAttribute ->
                    Succeeded value

                _ ->
                    Failed "Expecting null attribute ($)"
        )


{-| Decode the special 'derived value' attribute (`*`) as the given value.
-}
derivedValue : a -> Decoder Attribute a
derivedValue value =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                Types.DerivedValue ->
                    Succeeded value

                _ ->
                    Failed "Expecting 'derived value' attribute (*)"
        )


{-| Decode an attribute that may be null, returning `Nothing` if it is.
-}
optional : Decoder Attribute a -> Decoder Attribute (Maybe a)
optional decoder =
    Decoder
        (\inputAttribute ->
            case decodeResult decoder inputAttribute of
                Succeeded value ->
                    Succeeded (Just value)

                Failed message ->
                    case inputAttribute of
                        Types.NullAttribute ->
                            Succeeded Nothing

                        _ ->
                            Failed message

                UnexpectedType ->
                    Failed "Internal error: UnexpectedType from attribute decoder"
        )


{-| Run one decoder on some input, and based on the result of the first decoder
decide what decoder to apply next to the same input.
-}
andThen : (a -> Decoder i b) -> Decoder i a -> Decoder i b
andThen function decoder =
    Decoder
        (\input ->
            case decodeResult decoder input of
                Succeeded intermediateValue ->
                    decodeResult (function intermediateValue) input

                Failed message ->
                    Failed message

                UnexpectedType ->
                    UnexpectedType
        )


{-| You may run into cases where you end up with a decoder that returns another
decoder, perhaps if you construct a decoder within a call to `Decode.map2` or
similar. This function will 'flatten' the resulting decoder;

    Decode.flatten

is equivalent to

    Decode.andThen identity

-}
flatten : Decoder i (Decoder i a) -> Decoder i a
flatten decoder =
    decoder |> andThen Basics.identity


{-| Define a decoder lazily such that it is only constructed if needed. This is
primarily used to break circular reference chains between decoders.
-}
lazy : (() -> Decoder i a) -> Decoder i a
lazy constructor =
    Decoder (\input -> decodeResult (constructor ()) input)
