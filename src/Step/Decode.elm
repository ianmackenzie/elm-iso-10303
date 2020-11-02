module Step.Decode exposing
    ( Decoder, FileDecoder, File, EntityDecoder, AttributeListDecoder, AttributeDecoder, Error(..)
    , file, header, single, all
    , singleTopLevel, allTopLevel
    , entity
    , attribute
    , bool, int, float, string, enum, referenceTo, binaryData, null, optional, list, tuple2, tuple3, derivedValue
    , boolAs, intAs, floatAs, stringAs, enumAs, binaryDataAs, listAs
    , succeed, fail, map, map2, map3, map4, map5, map6, map7, map8, andThen, oneOf, lazy, identity, run
    )

{-| This module lets you decode data from STEP files in a similar way to how
you decode [JSON](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode).


# Decoder types

@docs Decoder, FileDecoder, File, EntityDecoder, AttributeListDecoder, AttributeDecoder, Error


# Decoding a file

@docs file, header, single, all


## Top-level entities

It may be useful in some cases to only decode _top-level_ entities (entities
that are not referenced by any other entities). These otherwise work just like
`single` and `all`.

@docs singleTopLevel, allTopLevel


# Decoding an entity

@docs entity


# Decoding attributes

@docs attribute

@docs bool, int, float, string, enum, referenceTo, binaryData, null, optional, list, tuple2, tuple3, derivedValue


## Typed attributes

@docs boolAs, intAs, floatAs, stringAs, enumAs, binaryDataAs, listAs


# Working with decoders

@docs succeed, fail, map, map2, map3, map4, map5, map6, map7, map8, andThen, oneOf, lazy, identity, run

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
import Step.Internal as Internal
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types as File exposing (Attribute, Entity, Header)


{-| A `Decoder` describes how to attempt to decode some input of type `i` (an
entire file, an individual entity, a specific attribute) and produce some output
of type `a`.

Entity decoders have the additional ability to choose whether to _match_ a
particular entity. For example, a decoder for `CARTESIAN_POINT` entities would
_fail_ if it encountered a `CARTESIAN_POINT` in an unexpected format (no XYZ
coordinates, for example) but would _not match_ a `DIRECTION` entity. This
distinction is important to avoid silently swallowing errors.

The `m` type parameter indicates whether a decoder has this ability to match or
not match inputs; it is equal to `String` for entity decoders and `Never`
otherwise.

You will rarely use the `Decoder` type directly in your own code, instead using
the specialized types such as `EntityDecoder`, but you will likely see it in
compiler error messages so it is useful to understand what the type parameters
mean.

-}
type Decoder i m a
    = Decoder (i -> DecodeResult m a)


{-| A `Decoder` that takes an entire STEP file as input and produces some
output.
-}
type alias FileDecoder a =
    Decoder File Never a


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


{-| A `Decoder` that takes a STEP entity as input, chooses whether or not it
should 'match' that entity (generally based on the entity's type), and then (if
it matches) decode that entity to produce some output.
-}
type alias EntityDecoder a =
    Decoder Entity String a


{-| A `Decoder` that takes a list of entity attributes as input and produces
some output. The only `AttributeListDecoder` is [`attribute`](#attribute), which
selects a particular attribute from the list by index and then applies a given
`AttributeDecoder` to that attribute.
-}
type alias AttributeListDecoder a =
    Decoder (List Attribute) Never a


{-| A `Decoder` that takes a single entity attribute as input and produces some
output.
-}
type alias AttributeDecoder a =
    Decoder Attribute Never a


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


type DecodeResult m a
    = Succeeded a
    | Failed String
    | NotMatched m


decodeResult : Decoder i m a -> i -> DecodeResult m a
decodeResult (Decoder function) input =
    function input


{-| Run a decoder on some input. You will not usually need to use this function
directly, but it may be necessary if you use [`Decode.identity`](#identity) to
access raw [`Entity`](Step-Types#Entity) or [`Attribute`](Step-Types#Attribute)
values.
-}
run : Decoder i m a -> i -> Result String a
run decoder input =
    case decodeResult decoder input of
        Succeeded value ->
            Ok value

        Failed message ->
            Err message

        NotMatched _ ->
            Err "Not matched"


{-| A special decoder that always succeeds with the given value. Primarily
useful with [`andThen`](#andThen).
-}
succeed : a -> Decoder i m a
succeed value =
    Decoder (always (Succeeded value))


{-| A special decoder that always fails with the given error message.
-}
fail : String -> Decoder i m a
fail description =
    Decoder (always (Failed description))


{-| A special decoder that just returns its input unmodified. This can be useful
if you want to get a raw [`Entity`](Step-Types#Entity) or [`Attribute`](Step-Types#Attribute)
value and inspect it.
-}
identity : Decoder i m i
identity =
    Decoder (\input -> Succeeded input)


{-| Decode a STEP file given as a `String` using the given decoder. For example,
to extract the file header and all `CARTESIAN_POINT` entities from a given file,
you might write:

    import Step.Types as Step
    import Step.Decode as Decode exposing
        ( Decoder
        , FileDecoder
        , EntityDecoder
        )

    type alias FileData =
        { header : Step.Header
        , points : List ( Float, Float, Float )
        }

    pointDecoder : EntityDecoder ( Float, Float, Float )
    pointDecoder =
        Decode.entity "CARTESIAN_POINT" <|
            Decode.attribute 1 (Decode.tuple3 Decode.float)

    fileDecoder : FileDecoder FileData
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
file : FileDecoder a -> String -> Result Error a
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

                    NotMatched notMatched ->
                        never notMatched
            )


{-| Extract the [header](Step-File#Header) of a STEP file.
-}
header : FileDecoder Header
header =
    Decoder
        (\(File properties) ->
            Succeeded properties.header
        )


{-| Attempt to find exactly one entity in a file that matches the given decoder.
If there are no matching entities or more than one matching entity, the decoder
will fail.
-}
single : EntityDecoder a -> FileDecoder a
single entityDecoder =
    Decoder
        (\(File properties) ->
            singleEntity entityDecoder properties.allEntities Nothing
        )


{-| Attempt to find exactly one top-level entity in a file that matches the
given decoder.
-}
singleTopLevel : EntityDecoder a -> FileDecoder a
singleTopLevel entityDecoder =
    Decoder
        (\(File properties) ->
            singleEntity entityDecoder properties.allEntities Nothing
        )


singleEntity : EntityDecoder a -> List Entity -> Maybe a -> DecodeResult Never a
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

                NotMatched _ ->
                    singleEntity decoder rest currentValue


{-| Find all entities in a file matching the given decoder.
-}
all : EntityDecoder a -> FileDecoder (List a)
all entityDecoder =
    Decoder
        (\(File properties) ->
            allEntities entityDecoder properties.allEntities []
        )


{-| Find all top-level entities in a file matching the given decoder.
-}
allTopLevel : EntityDecoder a -> FileDecoder (List a)
allTopLevel entityDecoder =
    Decoder
        (\(File properties) ->
            allEntities entityDecoder properties.topLevelEntities []
        )


allEntities : EntityDecoder a -> List Entity -> List a -> DecodeResult Never (List a)
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

                NotMatched _ ->
                    allEntities decoder rest accumulated


{-| Decode an entity of the given type name, using the given decoder on the
entity's list of attributes. If this decoder is passed to [`single`](#single) or
[`all`](#all), any entities that do not have the given type will be skipped.
-}
entity : String -> AttributeListDecoder a -> EntityDecoder a
entity givenTypeName decoder =
    let
        searchTypeName =
            TypeName.fromString givenTypeName
    in
    Decoder
        (\currentEntity ->
            case currentEntity of
                File.SimpleEntity typeName attributes ->
                    if typeName == searchTypeName then
                        case decodeResult decoder attributes of
                            Succeeded a ->
                                Succeeded a

                            Failed message ->
                                Failed message

                            NotMatched notMatched ->
                                never notMatched

                    else
                        NotMatched
                            ("Expected entity to have type '"
                                ++ TypeName.toString searchTypeName
                                ++ "', got '"
                                ++ TypeName.toString typeName
                                ++ "'"
                            )

                File.ComplexEntity entityRecords ->
                    partialEntity searchTypeName decoder entityRecords
        )


partialEntity : TypeName -> AttributeListDecoder a -> List ( TypeName, List Attribute ) -> DecodeResult String a
partialEntity searchTypeName decoder entityRecords =
    case entityRecords of
        [] ->
            NotMatched
                ("Complex entity has no sub-entity of type '"
                    ++ TypeName.toString searchTypeName
                    ++ "'"
                )

        ( typeName, attributes ) :: rest ->
            if typeName == searchTypeName then
                case decodeResult decoder attributes of
                    Succeeded value ->
                        Succeeded value

                    Failed message ->
                        Failed message

                    NotMatched notMatched ->
                        never notMatched

            else
                partialEntity searchTypeName decoder rest


{-| Decode a specific attribute by index (starting from 0) in an attribute list.
To decode data from multiple attributes in a list, you can use one of the `mapN`
functions, for example:

    type alias MyAttributes =
        { name : String
        , age : Int
        , height : Float
        }

    myAttributesDecoder : AttributeListDecoder MyAttributes
    myAttributesDecoder =
        Decode.map3 MyAttributes
            (Decode.attribute 0 Decode.string)
            (Decode.attribute 1 Decode.int)
            (Decode.attribute 2 Decode.float)

-}
attribute : Int -> AttributeDecoder a -> AttributeListDecoder a
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

                        NotMatched notMatched ->
                            never notMatched

                Nothing ->
                    Failed ("No attribute at index " ++ String.fromInt index)
        )


mapHelp : (a -> b) -> DecodeResult m a -> DecodeResult m b
mapHelp function result =
    case result of
        Succeeded value ->
            Succeeded (function value)

        Failed message ->
            Failed message

        NotMatched message ->
            NotMatched message


{-| Map the value produced by a decoder.
-}
map : (a -> b) -> Decoder i m a -> Decoder i m b
map mapFunction decoder =
    Decoder (\input -> mapHelp mapFunction (decodeResult decoder input))


map2Help : (a -> b -> c) -> DecodeResult m a -> DecodeResult m b -> DecodeResult m c
map2Help function resultA resultB =
    case resultA of
        Succeeded value ->
            mapHelp (function value) resultB

        NotMatched message ->
            NotMatched message

        Failed message ->
            Failed message


{-| Map over two decoders.
-}
map2 :
    (a -> b -> c)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
map2 function (Decoder functionA) (Decoder functionB) =
    Decoder (\input -> map2Help function (functionA input) (functionB input))


{-| Map over three decoders.
-}
map3 :
    (a -> b -> c -> d)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
map3 function decoderA decoderB decoderC =
    map2 (<|) (map2 function decoderA decoderB) decoderC


{-| Map over four decoders.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
    -> Decoder i m e
map4 function decoderA decoderB decoderC decoderD =
    map2 (<|) (map3 function decoderA decoderB decoderC) decoderD


{-| Map over five decoders.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
    -> Decoder i m e
    -> Decoder i m f
map5 function decoderA decoderB decoderC decoderD decoderE =
    map2 (<|) (map4 function decoderA decoderB decoderC decoderD) decoderE


{-| Map over six decoders.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
    -> Decoder i m e
    -> Decoder i m f
    -> Decoder i m g
map6 function decoderA decoderB decoderC decoderD decoderE decoderF =
    map2 (<|) (map5 function decoderA decoderB decoderC decoderD decoderE) decoderF


{-| Map over seven decoders.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
    -> Decoder i m e
    -> Decoder i m f
    -> Decoder i m g
    -> Decoder i m h
map7 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    map2 (<|) (map6 function decoderA decoderB decoderC decoderD decoderE decoderF) decoderG


{-| Map over eight decoders.
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> j)
    -> Decoder i m a
    -> Decoder i m b
    -> Decoder i m c
    -> Decoder i m d
    -> Decoder i m e
    -> Decoder i m f
    -> Decoder i m g
    -> Decoder i m h
    -> Decoder i m j
map8 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG decoderH =
    map2 (<|) (map7 function decoderA decoderB decoderC decoderD decoderE decoderF decoderG) decoderH


{-| Decode a single attribute as a `Bool` (from the special STEP enum values
`.T.` and `.F.`).
-}
bool : AttributeDecoder Bool
bool =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.BoolAttribute value ->
                    Succeeded value

                _ ->
                    Failed "Expected a bool"
        )


{-| Decode a single attribute as an `Int`.
-}
int : AttributeDecoder Int
int =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.IntAttribute value ->
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
float : AttributeDecoder Float
float =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.FloatAttribute value ->
                    Succeeded value

                File.NullAttribute ->
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
string : AttributeDecoder String
string =
    Decoder
        (\attribute_ ->
            case attribute_ of
                File.StringAttribute value ->
                    Succeeded value

                _ ->
                    Failed "Expected a string"
        )


{-| Decode a single attribute as a blob of binary data, using the given
[decoder](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode).
-}
binaryData : Bytes.Decode.Decoder a -> AttributeDecoder a
binaryData bytesDecoder =
    Decoder
        (\attribute_ ->
            case attribute_ of
                File.BinaryDataAttribute value ->
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

    lightColorDecoder : AttributeDecoder LightColor
    lightColorDecoder =
        Decode.enum
            [ ( "RED", Red )
            , ( "YELLOW", Yellow )
            , ( "GREEN", Green )
            ]

(Note that the given strings will be normalized, so you do not have to worry
about upper vs. lower case and you can either include or omit the leading and
trailing periods.)

-}
enum : List ( String, a ) -> AttributeDecoder a
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
                File.EnumAttribute enumValue ->
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


collectDecodedAttributes : AttributeDecoder a -> List a -> List Attribute -> DecodeResult Never (List a)
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

                NotMatched notMatched ->
                    never notMatched


{-| Decode an attribute as a list, passing the decoder to be used for each list
item.
-}
list : AttributeDecoder a -> AttributeDecoder (List a)
list itemDecoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.AttributeList attributes_ ->
                    collectDecodedAttributes itemDecoder [] attributes_

                _ ->
                    Failed "Expected a list"
        )


typedAttribute : String -> AttributeDecoder a -> AttributeDecoder a
typedAttribute givenTypeName decoder =
    let
        expectedTypeName =
            TypeName.fromString givenTypeName
    in
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.TypedAttribute attributeTypeName underlyingAttribute ->
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
boolAs : String -> AttributeDecoder Bool
boolAs givenTypeName =
    typedAttribute givenTypeName bool


{-| Decode an `Int` wrapped as the given type.
-}
intAs : String -> AttributeDecoder Int
intAs givenTypeName =
    typedAttribute givenTypeName int


{-| Decode a `Float` wrapped as the given type.
-}
floatAs : String -> AttributeDecoder Float
floatAs givenTypeName =
    typedAttribute givenTypeName float


{-| Decode a `String` wrapped as the given type.
-}
stringAs : String -> AttributeDecoder String
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
binaryDataAs : String -> Bytes.Decode.Decoder a -> AttributeDecoder a
binaryDataAs givenTypeName bytesDecoder =
    typedAttribute givenTypeName (binaryData bytesDecoder)


{-| Decode an enum wrapped as the given type.
-}
enumAs : String -> List ( String, a ) -> AttributeDecoder a
enumAs givenTypeName cases =
    typedAttribute givenTypeName (enum cases)


{-| Decode a list wrapped as the given type.
-}
listAs : String -> AttributeDecoder a -> AttributeDecoder (List a)
listAs givenTypeName itemDecoder =
    typedAttribute givenTypeName (list itemDecoder)


{-| Decode a list of exactly two elements, passing the decoder to be used for
the two elements.
-}
tuple2 : AttributeDecoder a -> AttributeDecoder ( a, a )
tuple2 decoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.AttributeList [ firstAttribute, secondAttribute ] ->
                    map2Help Tuple.pair
                        (decodeResult decoder firstAttribute)
                        (decodeResult decoder secondAttribute)

                _ ->
                    Failed "Expected a list of two items"
        )


{-| Decode a list of exactly three elements, passing the decoder to be used for
the three elements.
-}
tuple3 : AttributeDecoder a -> AttributeDecoder ( a, a, a )
tuple3 decoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.AttributeList [ firstAttribute, secondAttribute, thirdAttribute ] ->
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
referenceTo : EntityDecoder a -> AttributeDecoder a
referenceTo entityDecoder =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.ReferenceTo referencedEntity ->
                    case decodeResult entityDecoder referencedEntity of
                        Succeeded value ->
                            Succeeded value

                        Failed message ->
                            Failed message

                        NotMatched message ->
                            Failed message

                _ ->
                    Failed "Expected a referenced entity"
        )


{-| Construct an entity decoder that may match one of many entity types.
-}
oneOf : List (EntityDecoder a) -> EntityDecoder a
oneOf decoders =
    Decoder (oneOfHelp decoders [])


oneOfHelp : List (EntityDecoder a) -> List String -> Entity -> DecodeResult String a
oneOfHelp decoders matchErrors input =
    case decoders of
        [] ->
            -- No more decoders to try: fail with an error message that
            -- aggregates all the individual error messages
            NotMatched
                ("No decoders matched (error messages: \""
                    ++ String.join "\", \"" (List.reverse matchErrors)
                    ++ "\")"
                )

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
                NotMatched matchError ->
                    oneOfHelp rest (matchError :: matchErrors) input


{-| Decode the special 'null' attribute (`$`) as the given value.
-}
null : a -> AttributeDecoder a
null value =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.NullAttribute ->
                    Succeeded value

                _ ->
                    Failed "Expecting null attribute ($)"
        )


{-| Decode the special 'derived value' attribute (`*`) as the given value.
-}
derivedValue : a -> AttributeDecoder a
derivedValue value =
    Decoder
        (\inputAttribute ->
            case inputAttribute of
                File.DerivedValue ->
                    Succeeded value

                _ ->
                    Failed "Expecting 'derived value' attribute (*)"
        )


{-| Decode an attribute that may be null, returning `Nothing` if it is.
-}
optional : AttributeDecoder a -> AttributeDecoder (Maybe a)
optional decoder =
    Decoder
        (\inputAttribute ->
            case decodeResult decoder inputAttribute of
                Succeeded value ->
                    Succeeded (Just value)

                Failed message ->
                    case inputAttribute of
                        File.NullAttribute ->
                            Succeeded Nothing

                        _ ->
                            Failed message

                NotMatched notMatched ->
                    never notMatched
        )


{-| Run one decoder, and based on the result of the first decoder decide what
decoder to apply next. The only restriction is that this _second_ decoder must
be of a type that always matches (so not an [`entity`](#entity) decoder), since
it is the _first_ decoder that decides whether to match the input or not.
-}
andThen : (a -> Decoder i Never b) -> Decoder i m a -> Decoder i m b
andThen function decoder =
    Decoder
        (\input ->
            case decodeResult decoder input of
                Succeeded intermediateValue ->
                    case decodeResult (function intermediateValue) input of
                        Succeeded value ->
                            Succeeded value

                        Failed message ->
                            Failed message

                        NotMatched notMatched ->
                            never notMatched

                Failed message ->
                    Failed message

                NotMatched notMatched ->
                    NotMatched notMatched
        )


{-| Define a decoder lazily such that it is only constructed if needed. This is
primarily used to break circular reference chains between decoders.
-}
lazy : (() -> Decoder i m a) -> Decoder i m a
lazy constructor =
    Decoder (\input -> decodeResult (constructor ()) input)
