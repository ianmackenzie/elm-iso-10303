module Step.Decode exposing
    ( Decoder, File, Context
    , parse, header, single, all, get
    , simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12, simpleEntity13, simpleEntity14, simpleEntity15, simpleEntity16
    , complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6, complexEntity7, complexEntity8, complexEntity9, complexEntity10, complexEntity11, complexEntity12
    , subEntity0, subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6, subEntity7, subEntity8, subEntity9, subEntity10, subEntity11, subEntity12
    , oneOf
    , keepContext, ignoreContext
    , keepId, ignoreId
    , keep, ignore
    , bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId, referenceWithId
    , typedAttribute
    , map, validate, resolve, andThen, succeed, fail, lazy
    , chain, filter, inject, transform
    )

{-|

@docs Decoder, File, Context

@docs parse, header, single, all, get


# Entities

The `simpleEntity*` functions all attempt to decode a simple STEP entity with
the given number of attributes.

@docs simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12, simpleEntity13, simpleEntity14, simpleEntity15, simpleEntity16


## Complex entities

The `complexEntity*` functions all attempt to decode a complex STEP entity with
the given number of sub-entities.

@docs complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6, complexEntity7, complexEntity8, complexEntity9, complexEntity10, complexEntity11, complexEntity12

The `subEntity*` functions all attempt to decode a sub-entity (of a complex STEP
entity) with the given number of attributes.

@docs subEntity0, subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6, subEntity7, subEntity8, subEntity9, subEntity10, subEntity11, subEntity12


## Alternatives

@docs oneOf


## Context

@docs keepContext, ignoreContext


## Entity ID

@docs keepId, ignoreId


# Attributes

@docs keep, ignore

@docs bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId, referenceWithId

@docs typedAttribute


# Working with decoders

@docs map, validate, resolve, andThen, succeed, fail, lazy


## Context

@docs chain, filter, inject, transform

-}

import Array exposing (Array)
import Bytes.Decode
import Dict
import Regex exposing (Regex)
import Result
import Step.Bytes
import Step.Pattern as Pattern exposing (Pattern)
import Step.Preprocess as Preprocess
import Step.String
import Step.Types exposing (Attribute, Entity, Header, SubEntity)


{-| Represents an entire STEP file composed of a header and a list of entities.
You can [get the header](#header) from a file or extract data from it using
[entity decoders](#entities).
-}
type File
    = File Header (Array String)


{-| Represents a 'context' value that is threaded through decoding after being
passed in at the top level (and possibly updated/transformed during decoding).
-}
type Context
    = Context


{-| A `Decoder` describes how to attempt to decode some input of type `i` (an
entity or an attribute) and produce some output of type `a`.
-}
type Decoder input context output
    = Decoder Pattern (Chomp context output)


type alias Chomp context output =
    EntityLine context -> Submatches -> DecodeResult output Submatches


type alias EntityLine context =
    { file : File
    , context : context
    , id : Int
    , string : String
    }


type alias Submatches =
    List (Maybe String)


type alias Error =
    { entityIds : List Int
    , message : String
    }


type DecodeResult output remaining
    = Succeeded output remaining
    | Failed Error
    | UnexpectedType


errorMessage : String -> DecodeResult output remaining
errorMessage message =
    Failed
        { entityIds = []
        , message = message
        }


{-| Attempt to parse a given string as a STEP file.
-}
parse : String -> Result String File
parse contents =
    case Preprocess.split contents of
        Just ( parsedHeader, entityLines ) ->
            Ok (File parsedHeader entityLines)

        Nothing ->
            Err "Failed to parse STEP file header"


internalError : String -> DecodeResult output remaining
internalError message =
    errorMessage ("Internal error in STEP parsing: " ++ message)


{-| Get the [header](Step-Types#Header) of a given file.
-}
header : File -> Header
header file =
    let
        (File fileHeader _) =
            file
    in
    fileHeader


{-| Decode a single entity from a file by ID, using the given decoder. Usually
you will want to use [`single`](#single) instead.
-}
get : Int -> Decoder Entity context output -> File -> context -> Result String output
get id entityDecoder file givenContext =
    let
        (File _ entities) =
            file
    in
    case Array.get id entities of
        Nothing ->
            Err (noEntityWithId id)

        Just "" ->
            Err (noEntityWithId id)

        Just line ->
            let
                entityLine =
                    { file = file
                    , context = givenContext
                    , id = id
                    , string = line
                    }
            in
            case decodeEntity entityDecoder entityLine of
                Succeeded value () ->
                    Ok value

                Failed error ->
                    Err (reportError error)

                UnexpectedType ->
                    Err (reportError (entityHasUnexpectedType id))


{-| Find all entities in a file matching the given decoder.
-}
all : Decoder Entity context output -> File -> context -> Result String (List output)
all entityDecoder file givenContext =
    let
        (File _ entities) =
            file
    in
    Array.foldl (decodeAllHelp entityDecoder file givenContext) ( 0, Ok [] ) entities
        |> Tuple.second
        |> Result.map List.reverse


noEntityWithId : Int -> String
noEntityWithId id =
    "No entity with ID " ++ idString id


entityHasUnexpectedType : Int -> Error
entityHasUnexpectedType id =
    { entityIds = [ id ]
    , message = "Entity has unexpected type"
    }


{-| Attempt to find exactly one entity in a file that matches the given decoder.
If there are no matching entities or more than one matching entity, an error
message will be returned.
-}
single : Decoder Entity context output -> File -> context -> Result String output
single entityDecoder file givenContext =
    case all entityDecoder file givenContext of
        Ok [ result ] ->
            Ok result

        Ok [] ->
            Err "No matching entities found"

        Ok (_ :: _ :: _) ->
            Err "More than one matching entity found"

        Err message ->
            Err message


decodeEntity : Decoder Entity context output -> EntityLine context -> DecodeResult output ()
decodeEntity (Decoder _ chomp) entityLine =
    case chomp entityLine [] of
        Succeeded value [] ->
            Succeeded value ()

        Succeeded _ (_ :: _) ->
            wrongNumberOfSubmatches

        Failed { entityIds, message } ->
            let
                updatedEntityIds =
                    if List.head entityIds == Just entityLine.id then
                        entityIds

                    else
                        entityLine.id :: entityIds
            in
            Failed
                { entityIds = updatedEntityIds
                , message = message
                }

        UnexpectedType ->
            UnexpectedType


decodeAllHelp :
    Decoder Entity context output
    -> File
    -> context
    -> String
    -> ( Int, Result String (List output) )
    -> ( Int, Result String (List output) )
decodeAllHelp entityDecoder file givenContext entityString foldState =
    let
        ( id, currentResult ) =
            foldState
    in
    if entityString == "" then
        ( id + 1, currentResult )

    else
        case currentResult of
            Ok accumulated ->
                let
                    entityLine =
                        { file = file
                        , context = givenContext
                        , id = id
                        , string = entityString
                        }
                in
                case decodeEntity entityDecoder entityLine of
                    UnexpectedType ->
                        ( id + 1, currentResult )

                    Succeeded value () ->
                        ( id + 1, Ok (value :: accumulated) )

                    Failed error ->
                        ( id, Err (reportError error) )

            Err _ ->
                foldState


idString : Int -> String
idString id =
    "#" ++ String.fromInt id


reportError : Error -> String
reportError { entityIds, message } =
    let
        entityChain =
            List.map idString entityIds
                |> String.join "->"
    in
    "In " ++ entityChain ++ ": " ++ message


simpleEntityPattern : String -> List Pattern -> Pattern
simpleEntityPattern typeName attributePatterns =
    let
        attributeSeparatorPattern =
            Pattern.sequence
                [ Pattern.whitespace
                , Pattern.token ","
                , Pattern.whitespace
                ]

        attributesPattern =
            Pattern.railway attributeSeparatorPattern attributePatterns
    in
    Pattern.sequence
        [ Pattern.startOfInput
        , Pattern.token typeName
        , Pattern.whitespace
        , Pattern.token "("
        , Pattern.whitespace
        , attributesPattern
        , Pattern.whitespace
        , Pattern.token ")"
        , Pattern.endOfInput
        ]


simpleEntityRegex : String -> List Pattern -> Regex
simpleEntityRegex typeName attributePatterns =
    Pattern.compile (simpleEntityPattern typeName attributePatterns)


wrongNumberOfSubmatches : DecodeResult a r
wrongNumberOfSubmatches =
    internalError "wrong number of submatches"


makeEntityDecoder : Chomp context output -> Decoder Entity context output
makeEntityDecoder chomp =
    Decoder Pattern.startOfInput chomp


simpleEntityDecoder : a -> String -> List Pattern -> Chomp context (a -> output) -> Decoder Entity context output
simpleEntityDecoder callback typeName attributePatterns chompAttributes =
    let
        regex =
            simpleEntityRegex typeName attributePatterns
    in
    makeEntityDecoder <|
        \entityLine _ ->
            case Regex.find regex entityLine.string of
                [] ->
                    UnexpectedType

                [ { submatches } ] ->
                    case chompAttributes entityLine submatches of
                        Succeeded handler remaining ->
                            Succeeded (handler callback) remaining

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                _ :: _ :: _ ->
                    internalError "more than one regex match for a single entity"


{-| Get the current context while decoding.
-}
keepContext : Decoder Context context ((context -> a) -> a)
keepContext =
    Decoder Pattern.startOfInput <|
        \entityLine submatches ->
            Succeeded ((|>) entityLine.context) submatches


{-| Ignore the current context while decoding.
-}
ignoreContext : Decoder Context context (a -> a)
ignoreContext =
    Decoder Pattern.startOfInput <|
        \_ submatches ->
            Succeeded identity submatches


{-| Get the ID of an entity while decoding.
-}
keepId : Decoder Int context ((Int -> a) -> a)
keepId =
    Decoder Pattern.startOfInput <|
        \entityLine submatches ->
            Succeeded ((|>) entityLine.id) submatches


{-| Ignore the ID of an entity while decoding.
-}
ignoreId : Decoder Int context (a -> a)
ignoreId =
    Decoder Pattern.startOfInput <|
        \_ submatches ->
            Succeeded identity submatches


thenChomp : Chomp context (b -> c) -> Chomp context (a -> b) -> Chomp context (a -> c)
thenChomp chomp2 chomp1 =
    \entityLine submatches ->
        case chomp1 entityLine submatches of
            Succeeded result1 after1 ->
                case chomp2 entityLine after1 of
                    Succeeded result2 after2 ->
                        Succeeded (result1 >> result2) after2

                    Failed error ->
                        Failed error

                    UnexpectedType ->
                        UnexpectedType

            Failed error ->
                Failed error

            UnexpectedType ->
                UnexpectedType


{-| -}
simpleEntity1 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> output)
    -> Decoder Entity context output
simpleEntity1 callback contextDecoder idDecoder typeName firstAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        attributePatterns =
            [ pattern1 ]

        chomp =
            chompContext |> thenChomp chompId |> thenChomp chomp1
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity2 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> output)
    -> Decoder Entity context output
simpleEntity2 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        attributePatterns =
            [ pattern1, pattern2 ]

        chomp =
            chompContext |> thenChomp chompId |> thenChomp chomp1 |> thenChomp chomp2
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity3 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> output)
    -> Decoder Entity context output
simpleEntity3 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        attributePatterns =
            [ pattern1, pattern2, pattern3 ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity4 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> output)
    -> Decoder Entity context output
simpleEntity4 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity5 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> output)
    -> Decoder Entity context output
simpleEntity5 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity6 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> output)
    -> Decoder Entity context output
simpleEntity6 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity7 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> output)
    -> Decoder Entity context output
simpleEntity7 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity8 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> output)
    -> Decoder Entity context output
simpleEntity8 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity9 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> output)
    -> Decoder Entity context output
simpleEntity9 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity10 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> output)
    -> Decoder Entity context output
simpleEntity10 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity11 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> output)
    -> Decoder Entity context output
simpleEntity11 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity12 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder Attribute context (m -> output)
    -> Decoder Entity context output
simpleEntity12 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity13 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder Attribute context (m -> n)
    -> Decoder Attribute context (n -> output)
    -> Decoder Entity context output
simpleEntity13 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder thirteenthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        (Decoder pattern13 chomp13) =
            thirteenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            , pattern13
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
                |> thenChomp chomp13
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity14 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder Attribute context (m -> n)
    -> Decoder Attribute context (n -> o)
    -> Decoder Attribute context (o -> output)
    -> Decoder Entity context output
simpleEntity14 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder thirteenthAttributeDecoder fourteenthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        (Decoder pattern13 chomp13) =
            thirteenthAttributeDecoder

        (Decoder pattern14 chomp14) =
            fourteenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            , pattern13
            , pattern14
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
                |> thenChomp chomp13
                |> thenChomp chomp14
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity15 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder Attribute context (m -> n)
    -> Decoder Attribute context (n -> o)
    -> Decoder Attribute context (o -> p)
    -> Decoder Attribute context (p -> output)
    -> Decoder Entity context output
simpleEntity15 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder thirteenthAttributeDecoder fourteenthAttributeDecoder fifteenthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        (Decoder pattern13 chomp13) =
            thirteenthAttributeDecoder

        (Decoder pattern14 chomp14) =
            fourteenthAttributeDecoder

        (Decoder pattern15 chomp15) =
            fifteenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            , pattern13
            , pattern14
            , pattern15
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
                |> thenChomp chomp13
                |> thenChomp chomp14
                |> thenChomp chomp15
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


{-| -}
simpleEntity16 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> String
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder Attribute context (m -> n)
    -> Decoder Attribute context (n -> o)
    -> Decoder Attribute context (o -> p)
    -> Decoder Attribute context (p -> q)
    -> Decoder Attribute context (q -> output)
    -> Decoder Entity context output
simpleEntity16 callback contextDecoder idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder thirteenthAttributeDecoder fourteenthAttributeDecoder fifteenthAttributeDecoder sixteenthAttributeDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        (Decoder pattern13 chomp13) =
            thirteenthAttributeDecoder

        (Decoder pattern14 chomp14) =
            fourteenthAttributeDecoder

        (Decoder pattern15 chomp15) =
            fifteenthAttributeDecoder

        (Decoder pattern16 chomp16) =
            sixteenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            , pattern13
            , pattern14
            , pattern15
            , pattern16
            ]

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
                |> thenChomp chomp13
                |> thenChomp chomp14
                |> thenChomp chomp15
                |> thenChomp chomp16
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


subEntityDecoder : String -> List Pattern -> Chomp context (a -> b) -> Decoder SubEntity context (a -> b)
subEntityDecoder typeName attributePatterns chompAttributes =
    let
        attributeSeparatorPattern =
            Pattern.sequence
                [ Pattern.whitespace
                , Pattern.token ","
                , Pattern.whitespace
                ]

        pattern =
            Pattern.sequence
                [ Pattern.capture (Pattern.token typeName)
                , Pattern.whitespace
                , Pattern.token "("
                , Pattern.whitespace
                , Pattern.railway attributeSeparatorPattern attributePatterns
                , Pattern.whitespace
                , Pattern.token ")"
                ]
    in
    Decoder pattern
        (\entityLine submatches ->
            case submatches of
                -- Type name was matched, proceed to chomp attributes
                (Just _) :: rest ->
                    case chompAttributes entityLine rest of
                        Succeeded handler remaining ->
                            Succeeded handler remaining

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                -- Type name was not matched
                Nothing :: _ ->
                    UnexpectedType

                [] ->
                    wrongNumberOfSubmatches
        )


{-| -}
subEntity0 : String -> Decoder SubEntity context (a -> a)
subEntity0 typeName =
    subEntityDecoder typeName [] (\_ submatches -> Succeeded identity submatches)


{-| -}
subEntity1 : String -> Decoder Attribute context (a -> b) -> Decoder SubEntity context (a -> b)
subEntity1 typeName firstAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder
    in
    subEntityDecoder typeName [ pattern1 ] chomp1


{-| -}
subEntity2 : String -> Decoder Attribute context (a -> b) -> Decoder Attribute context (b -> c) -> Decoder SubEntity context (a -> c)
subEntity2 typeName firstAttributeDecoder secondAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        attributePatterns =
            [ pattern1, pattern2 ]

        chompAttributes =
            chomp1 |> thenChomp chomp2
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity3 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder SubEntity context (a -> d)
subEntity3 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity4 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder SubEntity context (a -> e)
subEntity4 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity5 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder SubEntity context (a -> f)
subEntity5 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity6 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder SubEntity context (a -> g)
subEntity6 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity7 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder SubEntity context (a -> h)
subEntity7 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity8 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder SubEntity context (a -> i)
subEntity8 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity9 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder SubEntity context (a -> j)
subEntity9 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity10 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder SubEntity context (a -> k)
subEntity10 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity11 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder SubEntity context (a -> l)
subEntity11 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
    in
    subEntityDecoder typeName attributePatterns chompAttributes


{-| -}
subEntity12 :
    String
    -> Decoder Attribute context (a -> b)
    -> Decoder Attribute context (b -> c)
    -> Decoder Attribute context (c -> d)
    -> Decoder Attribute context (d -> e)
    -> Decoder Attribute context (e -> f)
    -> Decoder Attribute context (f -> g)
    -> Decoder Attribute context (g -> h)
    -> Decoder Attribute context (h -> i)
    -> Decoder Attribute context (i -> j)
    -> Decoder Attribute context (j -> k)
    -> Decoder Attribute context (k -> l)
    -> Decoder Attribute context (l -> m)
    -> Decoder SubEntity context (a -> m)
subEntity12 typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        (Decoder pattern3 chomp3) =
            thirdAttributeDecoder

        (Decoder pattern4 chomp4) =
            fourthAttributeDecoder

        (Decoder pattern5 chomp5) =
            fifthAttributeDecoder

        (Decoder pattern6 chomp6) =
            sixthAttributeDecoder

        (Decoder pattern7 chomp7) =
            seventhAttributeDecoder

        (Decoder pattern8 chomp8) =
            eighthAttributeDecoder

        (Decoder pattern9 chomp9) =
            ninthAttributeDecoder

        (Decoder pattern10 chomp10) =
            tenthAttributeDecoder

        (Decoder pattern11 chomp11) =
            eleventhAttributeDecoder

        (Decoder pattern12 chomp12) =
            twelfthAttributeDecoder

        attributePatterns =
            [ pattern1
            , pattern2
            , pattern3
            , pattern4
            , pattern5
            , pattern6
            , pattern7
            , pattern8
            , pattern9
            , pattern10
            , pattern11
            , pattern12
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
    in
    subEntityDecoder typeName attributePatterns chompAttributes


complexEntityRegex : List Pattern -> Regex
complexEntityRegex subEntityPatterns =
    Pattern.compile <|
        Pattern.sequence
            [ Pattern.startOfInput
            , Pattern.token "("
            , Pattern.whitespace
            , Pattern.railway Pattern.whitespace subEntityPatterns
            , Pattern.whitespace
            , Pattern.token ")"
            , Pattern.endOfInput
            ]


complexEntityDecoder : a -> List Pattern -> Chomp context (a -> b) -> Decoder Entity context b
complexEntityDecoder callback subEntityPatterns chomp =
    let
        regex =
            complexEntityRegex subEntityPatterns
    in
    makeEntityDecoder <|
        \entityLine _ ->
            case Regex.find regex entityLine.string of
                [] ->
                    UnexpectedType

                [ { submatches } ] ->
                    case chomp entityLine submatches of
                        Succeeded handler remaining ->
                            Succeeded (handler callback) remaining

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            UnexpectedType

                _ :: _ :: _ ->
                    internalError "more than one regex match for a single entity"


{-| -}
complexEntity1 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> output)
    -> Decoder Entity context output
complexEntity1 callback contextDecoder idDecoder firstSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        chomp =
            chompContext |> thenChomp chompId |> thenChomp chomp1
    in
    complexEntityDecoder callback [ pattern1 ] chomp


{-| -}
complexEntity2 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> output)
    -> Decoder Entity context output
complexEntity2 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        chomp =
            chompContext |> thenChomp chompId |> thenChomp chomp1 |> thenChomp chomp2
    in
    complexEntityDecoder callback [ pattern1, pattern2 ] chomp


{-| -}
complexEntity3 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> output)
    -> Decoder Entity context output
complexEntity3 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3 ]
        chomp


{-| -}
complexEntity4 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> output)
    -> Decoder Entity context output
complexEntity4 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4 ]
        chomp


{-| -}
complexEntity5 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> output)
    -> Decoder Entity context output
complexEntity5 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5 ]
        chomp


{-| -}
complexEntity6 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> output)
    -> Decoder Entity context output
complexEntity6 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6 ]
        chomp


{-| -}
complexEntity7 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> output)
    -> Decoder Entity context output
complexEntity7 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7 ]
        chomp


{-| -}
complexEntity8 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> i)
    -> Decoder SubEntity context (i -> output)
    -> Decoder Entity context output
complexEntity8 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder eighthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        (Decoder pattern8 chomp8) =
            eighthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8 ]
        chomp


{-| -}
complexEntity9 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> i)
    -> Decoder SubEntity context (i -> j)
    -> Decoder SubEntity context (j -> output)
    -> Decoder Entity context output
complexEntity9 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder eighthSubEntityDecoder ninthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        (Decoder pattern8 chomp8) =
            eighthSubEntityDecoder

        (Decoder pattern9 chomp9) =
            ninthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8, pattern9 ]
        chomp


{-| -}
complexEntity10 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> i)
    -> Decoder SubEntity context (i -> j)
    -> Decoder SubEntity context (j -> k)
    -> Decoder SubEntity context (k -> output)
    -> Decoder Entity context output
complexEntity10 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder eighthSubEntityDecoder ninthSubEntityDecoder tenthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        (Decoder pattern8 chomp8) =
            eighthSubEntityDecoder

        (Decoder pattern9 chomp9) =
            ninthSubEntityDecoder

        (Decoder pattern10 chomp10) =
            tenthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8, pattern9, pattern10 ]
        chomp


{-| -}
complexEntity11 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> i)
    -> Decoder SubEntity context (i -> j)
    -> Decoder SubEntity context (j -> k)
    -> Decoder SubEntity context (k -> l)
    -> Decoder SubEntity context (l -> output)
    -> Decoder Entity context output
complexEntity11 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder eighthSubEntityDecoder ninthSubEntityDecoder tenthSubEntityDecoder eleventhSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        (Decoder pattern8 chomp8) =
            eighthSubEntityDecoder

        (Decoder pattern9 chomp9) =
            ninthSubEntityDecoder

        (Decoder pattern10 chomp10) =
            tenthSubEntityDecoder

        (Decoder pattern11 chomp11) =
            eleventhSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8, pattern9, pattern10, pattern11 ]
        chomp


{-| -}
complexEntity12 :
    callback
    -> Decoder Context context (callback -> a)
    -> Decoder Int context (a -> b)
    -> Decoder SubEntity context (b -> c)
    -> Decoder SubEntity context (c -> d)
    -> Decoder SubEntity context (d -> e)
    -> Decoder SubEntity context (e -> f)
    -> Decoder SubEntity context (f -> g)
    -> Decoder SubEntity context (g -> h)
    -> Decoder SubEntity context (h -> i)
    -> Decoder SubEntity context (i -> j)
    -> Decoder SubEntity context (j -> k)
    -> Decoder SubEntity context (k -> l)
    -> Decoder SubEntity context (l -> m)
    -> Decoder SubEntity context (m -> output)
    -> Decoder Entity context output
complexEntity12 callback contextDecoder idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder seventhSubEntityDecoder eighthSubEntityDecoder ninthSubEntityDecoder tenthSubEntityDecoder eleventhSubEntityDecoder twelfthSubEntityDecoder =
    let
        (Decoder _ chompContext) =
            contextDecoder

        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        (Decoder pattern4 chomp4) =
            fourthSubEntityDecoder

        (Decoder pattern5 chomp5) =
            fifthSubEntityDecoder

        (Decoder pattern6 chomp6) =
            sixthSubEntityDecoder

        (Decoder pattern7 chomp7) =
            seventhSubEntityDecoder

        (Decoder pattern8 chomp8) =
            eighthSubEntityDecoder

        (Decoder pattern9 chomp9) =
            ninthSubEntityDecoder

        (Decoder pattern10 chomp10) =
            tenthSubEntityDecoder

        (Decoder pattern11 chomp11) =
            eleventhSubEntityDecoder

        (Decoder pattern12 chomp12) =
            twelfthSubEntityDecoder

        chomp =
            chompContext
                |> thenChomp chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
                |> thenChomp chomp8
                |> thenChomp chomp9
                |> thenChomp chomp10
                |> thenChomp chomp11
                |> thenChomp chomp12
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8, pattern9, pattern10, pattern11, pattern12 ]
        chomp


couldNotParseAttributeAs : String -> DecodeResult a Submatches
couldNotParseAttributeAs dataType =
    errorMessage ("Could not parse attribute as " ++ dataType)


{-| Decode a single attribute as a `Bool` (from the special STEP enum values
`.T.` and `.F.`).
-}
bool : Decoder Attribute context Bool
bool =
    Decoder Pattern.bool <|
        \_ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    Succeeded (submatch == ".T.") rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "bool"

                [] ->
                    wrongNumberOfSubmatches


{-| Decode a single attribute as a `String`.
-}
string : Decoder Attribute context String
string =
    Decoder Pattern.string <|
        \_ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    Succeeded (Step.String.decode (String.slice 1 -1 submatch)) rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "string"

                [] ->
                    wrongNumberOfSubmatches


{-| Decode a single attribute as a string which must be empty (whitespace only).
Useful when used with [`ignore`](#ignore) to check that the string being ignored
is in fact empty.
-}
emptyString : Decoder Attribute context ()
emptyString =
    Decoder Pattern.emptyString <|
        \_ submatches ->
            case submatches of
                (Just _) :: rest ->
                    Succeeded () rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "empty string"

                [] ->
                    wrongNumberOfSubmatches


{-| Decode a single attribute as a blob of binary data, using the given
[binary decoder](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode).
-}
binaryData : Bytes.Decode.Decoder a -> Decoder Attribute context a
binaryData bytesDecoder =
    Decoder Pattern.binaryData <|
        \_ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case Bytes.Decode.decode bytesDecoder (Step.Bytes.decode (String.slice 1 -1 submatch)) of
                        Just result ->
                            Succeeded result rest

                        Nothing ->
                            errorMessage "Decoding of binary data failed"

                Nothing :: _ ->
                    couldNotParseAttributeAs "binary data"

                [] ->
                    internalError "could not decode binary attribute"


{-| Decode a single attribute as a `Float`. Note that unlike JSON, STEP has
different encodings for `Int` and `Float` values so you cannot use this
function to decode integer values; if you want to decode an integer value as a
`Float` you will need to use

    Decode.map toFloat Decode.int

-}
float : Decoder Attribute context Float
float =
    Decoder Pattern.float <|
        \_ submatches ->
            case submatches of
                (Just floatString) :: rest ->
                    case String.toFloat floatString of
                        Just floatValue ->
                            Succeeded floatValue rest

                        Nothing ->
                            internalError ("could not parse '" ++ floatString ++ "' as a real number")

                Nothing :: _ ->
                    couldNotParseAttributeAs "float"

                [] ->
                    wrongNumberOfSubmatches


{-| Decode a single attribute as an `Int`.
-}
int : Decoder Attribute context Int
int =
    Decoder Pattern.int <|
        \_ submatches ->
            case submatches of
                (Just intString) :: rest ->
                    case String.toInt intString of
                        Just intValue ->
                            Succeeded intValue rest

                        Nothing ->
                            internalError ("could not parse '" ++ intString ++ "' as an integer")

                Nothing :: _ ->
                    couldNotParseAttributeAs "integer"

                [] ->
                    wrongNumberOfSubmatches


unexpectedTypeFromAttribute : DecodeResult output remaining
unexpectedTypeFromAttribute =
    internalError "attribute decoder returned UnexpectedType"


{-| Keep a particular attribute value to pass to the callback function for the
entity being decoded.
-}
keep : Decoder Attribute context a -> Decoder Attribute context ((a -> b) -> b)
keep attributeDecoder =
    let
        (Decoder pattern chompAttribute) =
            attributeDecoder
    in
    Decoder pattern
        (\entityLine submatches ->
            case chompAttribute entityLine submatches of
                Succeeded value remaining ->
                    Succeeded ((|>) value) remaining

                Failed error ->
                    Failed error

                UnexpectedType ->
                    unexpectedTypeFromAttribute
        )


{-| Ignore a particular attribute so that it does not get passed to the callback
function for the entity being decoded.
-}
ignore : Decoder Attribute context a -> Decoder Attribute context (b -> b)
ignore attributeDecoder =
    let
        (Decoder pattern _) =
            attributeDecoder
    in
    Decoder (Pattern.capture pattern) <|
        \_ submatches ->
            case submatches of
                (Just _) :: remaining ->
                    Succeeded identity remaining

                Nothing :: _ ->
                    errorMessage "Could not parse ignored attribute"

                [] ->
                    wrongNumberOfSubmatches


{-| Decode an attribute as a list, passing the decoder to be used for each list
item.
-}
list : Decoder Attribute context a -> Decoder Attribute context (List a)
list itemDecoder =
    let
        (Decoder itemPattern chompItem) =
            itemDecoder

        searchPattern =
            Pattern.sequence
                [ Pattern.whitespace
                , itemPattern
                , Pattern.whitespace
                , Pattern.oneOf [ Pattern.token ",", Pattern.endOfInput ]
                ]

        searchRegex =
            Pattern.compile searchPattern
    in
    Decoder (Pattern.list itemPattern) <|
        \entityLine submatches ->
            case submatches of
                (Just listString) :: rest ->
                    listHelp chompItem entityLine (Regex.find searchRegex (String.slice 1 -1 listString)) [] rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "list"

                [] ->
                    wrongNumberOfSubmatches


listHelp :
    Chomp context output
    -> EntityLine context
    -> List Regex.Match
    -> List output
    -> List (Maybe String)
    -> DecodeResult (List output) Submatches
listHelp chompItem entityLine matches accumulated remainingSubmatches =
    case matches of
        { submatches } :: rest ->
            case chompItem entityLine submatches of
                Succeeded item [] ->
                    listHelp chompItem entityLine rest (item :: accumulated) remainingSubmatches

                Succeeded _ (_ :: _) ->
                    wrongNumberOfSubmatches

                Failed error ->
                    Failed error

                UnexpectedType ->
                    unexpectedTypeFromAttribute

        [] ->
            Succeeded (List.reverse accumulated) remainingSubmatches


{-| Decode a list of exactly two elements, passing the decoder to be used for
the two elements.
-}
tuple2 : Decoder Attribute context a -> Decoder Attribute context ( a, a )
tuple2 itemDecoder =
    list itemDecoder
        |> validate
            (\items ->
                case items of
                    [ first, second ] ->
                        Ok ( first, second )

                    _ ->
                        Err ("Expected 2 items in list, found " ++ String.fromInt (List.length items))
            )


{-| Decode a list of exactly three elements, passing the decoder to be used for
the two elements.
-}
tuple3 : Decoder Attribute context a -> Decoder Attribute context ( a, a, a )
tuple3 itemDecoder =
    list itemDecoder
        |> validate
            (\items ->
                case items of
                    [ first, second, third ] ->
                        Ok ( first, second, third )

                    _ ->
                        Err ("Expected 3 items in list, found " ++ String.fromInt (List.length items))
            )


{-| Decode just the ID from an entity reference attribute. This may be useful
for deferred decoding/processing.
-}
referencedId : Decoder Attribute context Int
referencedId =
    Decoder Pattern.referencedId
        (\_ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case String.toInt (String.dropLeft 1 submatch) of
                        Just id ->
                            Succeeded id rest

                        Nothing ->
                            internalError ("failed to parse referenced ID '" ++ submatch ++ "'")

                Nothing :: _ ->
                    couldNotParseAttributeAs "entity reference"

                [] ->
                    wrongNumberOfSubmatches
        )


dropId : Int -> a -> a
dropId _ value =
    value


{-| Decode an attribute which is a reference to another entity, by providing the
decoder to use for that entity.
-}
referenceTo : Decoder Entity context a -> Decoder Attribute context a
referenceTo entityDecoder =
    referenceWithId dropId entityDecoder


{-| Decode an attribute which is a reference to another entity, and additionally
get the ID of that entity. You will need to pass a function that combines the
ID and decoded value into whatever you want; for example if you pass
`Tuple.pair` then you will get an `( Int, a )` value back.
-}
referenceWithId : (Int -> a -> b) -> Decoder Entity context a -> Decoder Attribute context b
referenceWithId combine entityDecoder =
    Decoder Pattern.referencedId
        (\entityLine submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case String.toInt (String.dropLeft 1 submatch) of
                        Just childId ->
                            let
                                (File _ entities) =
                                    entityLine.file
                            in
                            case Array.get childId entities of
                                Just childEntityString ->
                                    let
                                        childEntityLine =
                                            { file = entityLine.file
                                            , context = entityLine.context
                                            , id = childId
                                            , string = childEntityString
                                            }
                                    in
                                    case decodeEntity entityDecoder childEntityLine of
                                        Succeeded value () ->
                                            Succeeded (combine childId value) rest

                                        UnexpectedType ->
                                            Failed (entityHasUnexpectedType childId)

                                        Failed error ->
                                            Failed error

                                Nothing ->
                                    errorMessage (noEntityWithId childId)

                        Nothing ->
                            internalError ("failed to parse referenced ID '" ++ submatch ++ "'")

                Nothing :: _ ->
                    couldNotParseAttributeAs "entity reference"

                [] ->
                    wrongNumberOfSubmatches
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
enum : List ( String, a ) -> Decoder Attribute context a
enum cases =
    let
        normalized =
            List.map (Tuple.mapFirst String.toUpper) cases

        lookupDict =
            Dict.fromList normalized
    in
    Decoder Pattern.enum
        (\_ submatches ->
            case submatches of
                (Just enumString) :: remaining ->
                    case Dict.get (String.slice 1 -1 enumString) lookupDict of
                        Just value ->
                            Succeeded value remaining

                        Nothing ->
                            errorMessage ("unexpected enum value '" ++ enumString ++ "'")

                Nothing :: _ ->
                    couldNotParseAttributeAs "enum"

                [] ->
                    wrongNumberOfSubmatches
        )


{-| Decode a 'typed attribute'; for example if someone's age was stored as an
integer, then it might be encoded directly as an integer like `38` or as a typed
integer like `AGE(38)` or `YEARS(38)` or similar.
-}
typedAttribute : String -> Decoder Attribute context a -> Decoder Attribute context a
typedAttribute typeName (Decoder pattern chomp) =
    Decoder (Pattern.typedAttribute typeName pattern) chomp


{-| Map the value produced by a decoder.
-}
map : (a -> b) -> Decoder input context a -> Decoder input context b
map function decoder =
    let
        (Decoder pattern chomp) =
            decoder
    in
    Decoder pattern
        (\entityLine submatches ->
            case chomp entityLine submatches of
                Succeeded value remaining ->
                    Succeeded (function value) remaining

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType
        )


{-| Based on the result of one entity decoder, produce a second decoder to run
on the same entity.

_**WARNING**_: This is sometimes necessary but can be a major performance issue!
Wherever possible, ensure decoders are created _once_ instead of on the fly
inside an `andThen` callback. For example, in some cases you may be able to
create a few static decoders and then have logic inside and `andThen` simply
choose which of those to use.

-}
andThen : (a -> Decoder Entity context b) -> Decoder Entity context a -> Decoder Entity context b
andThen function firstDecoder =
    makeEntityDecoder <|
        \entityLine _ ->
            case decodeEntity firstDecoder entityLine of
                Succeeded firstValue () ->
                    let
                        secondDecoder =
                            function firstValue
                    in
                    case decodeEntity secondDecoder entityLine of
                        Succeeded secondValue () ->
                            Succeeded secondValue []

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            UnexpectedType

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType


{-| Transform the result of a decoder using its context. This works just like
`map` but additionally passes the current context to the mapping function.
-}
transform : (context -> a -> b) -> Decoder input context a -> Decoder input context b
transform function decoder =
    let
        (Decoder pattern chomp) =
            decoder
    in
    Decoder pattern
        (\entityLine submatches ->
            case chomp entityLine submatches of
                Succeeded value remaining ->
                    Succeeded (function entityLine.context value) remaining

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType
        )


{-| Take a decoder that expects a particular type of context, and 'inject' part
of the current context into it.
-}
inject :
    (outerContext -> innerContext)
    -> Decoder input innerContext output
    -> Decoder input outerContext output
inject selectContext decoder =
    let
        (Decoder pattern chomp) =
            decoder
    in
    Decoder pattern <|
        \entityLine submatches ->
            chomp
                { file = entityLine.file
                , context = selectContext entityLine.context
                , id = entityLine.id
                , string = entityLine.string
                }
                submatches


{-| Chain two decoders together so that the output of the first can affect the
behavior of the second. Given two decoders, you must additionally pass a
function that takes the context of the first decoder and its output, and
produces the context to feed into the second decoder. The result is a combined
decoder that accepts the context of the first and produces the output of the
second.
-}
chain :
    (context1 -> output1 -> context2)
    -> Decoder Entity context1 output1
    -> Decoder Entity context2 output2
    -> Decoder Entity context1 output2
chain function firstDecoder secondDecoder =
    makeEntityDecoder <|
        \entityLine _ ->
            case decodeEntity firstDecoder entityLine of
                Succeeded firstValue () ->
                    let
                        updatedEntityLine =
                            { file = entityLine.file
                            , context = function entityLine.context firstValue
                            , id = entityLine.id
                            , string = entityLine.string
                            }
                    in
                    case decodeEntity secondDecoder updatedEntityLine of
                        Succeeded secondValue () ->
                            Succeeded secondValue []

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            UnexpectedType

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType


{-| A variant of [`chain`](#chain) that additionally allows you to stop decoding
after the first step (never run the second decoder) by returning `Nothing`
instead of `Just context` from the callback function. In that case the output
of the combined decoder will also be `Nothing`.
-}
filter :
    (context1 -> output1 -> Maybe context2)
    -> Decoder Entity context1 output1
    -> Decoder Entity context2 output2
    -> Decoder Entity context1 (Maybe output2)
filter function firstDecoder secondDecoder =
    makeEntityDecoder <|
        \entityLine _ ->
            case decodeEntity firstDecoder entityLine of
                Succeeded firstValue () ->
                    case function entityLine.context firstValue of
                        Just secondContext ->
                            let
                                updatedEntityLine =
                                    { file = entityLine.file
                                    , context = secondContext
                                    , id = entityLine.id
                                    , string = entityLine.string
                                    }
                            in
                            case decodeEntity secondDecoder updatedEntityLine of
                                Succeeded secondValue () ->
                                    Succeeded (Just secondValue) []

                                Failed error ->
                                    Failed error

                                UnexpectedType ->
                                    UnexpectedType

                        Nothing ->
                            Succeeded Nothing []

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType


{-| A trivial decoder that always succeeds with the given value. May be useful
in combination with `andThen`, but consider using `validate` instead.
-}
succeed : a -> Decoder Entity context a
succeed value =
    makeEntityDecoder (\_ _ -> Succeeded value [])


{-| A trivial decoder that always failes with the given error message. May be
useful in combination with `andThen`, but consider using `validate` instead.
-}
fail : String -> Decoder Entity context a
fail message =
    makeEntityDecoder (\_ _ -> errorMessage message)


{-| Post-process the result of a decoder, either succeeding with a new value
(possibly of a different type) or failing with an error message. This is a
restricted form of `andThen` that does not have the same performance concerns,
so use `validate` instead of `andThen` wherever possible.
-}
validate : (a -> Result String b) -> Decoder input context a -> Decoder input context b
validate function decoder =
    let
        (Decoder pattern chomp) =
            decoder
    in
    Decoder pattern
        (\entityLine submatches ->
            case chomp entityLine submatches of
                Succeeded value remaining ->
                    case function value of
                        Ok result ->
                            Succeeded result remaining

                        Err message ->
                            errorMessage message

                Failed error ->
                    Failed error

                UnexpectedType ->
                    UnexpectedType
        )


{-| If a decoder produces a `Result String a` instead of simply a value of type
`a` (for example if the callback function can fail) then this can 'resolve' that
back into a regular decoder. It is equivalent to (and implemented as)
`validate identity`.
-}
resolve : Decoder input context (Result String output) -> Decoder input context output
resolve decoder =
    validate identity decoder


{-| Construct an entity decoder that tries several other entity decoders in
sequence.
-}
oneOf : List (Decoder Entity context a) -> Decoder Entity context a
oneOf entityDecoders =
    makeEntityDecoder (chompOneOf entityDecoders)


chompOneOf :
    List (Decoder Entity context output)
    -> EntityLine context
    -> Submatches
    -> DecodeResult output Submatches
chompOneOf entityDecoders entityLine _ =
    case entityDecoders of
        first :: rest ->
            case decodeEntity first entityLine of
                Succeeded value () ->
                    Succeeded value []

                UnexpectedType ->
                    chompOneOf rest entityLine []

                Failed error ->
                    Failed error

        [] ->
            UnexpectedType


{-| Decode the special 'null' attribute (`$`) as the given value.
-}
null : a -> Decoder Attribute context a
null result =
    Decoder Pattern.null
        (\_ submatches ->
            case submatches of
                (Just _) :: rest ->
                    Succeeded result rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "null ($)"

                [] ->
                    wrongNumberOfSubmatches
        )


{-| Decode the special 'derived value' attribute (`*`) as the given value.
-}
derivedValue : a -> Decoder Attribute context a
derivedValue result =
    Decoder Pattern.derivedValue
        (\_ submatches ->
            case submatches of
                (Just _) :: rest ->
                    Succeeded result rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "derived value (*)"

                [] ->
                    wrongNumberOfSubmatches
        )


{-| Decode an attribute that may be null, returning `Nothing` if it is.
-}
optional : Decoder Attribute context output -> Decoder Attribute context (Maybe output)
optional decoder =
    let
        (Decoder attributePattern chompItem) =
            decoder
    in
    Decoder (Pattern.optional attributePattern)
        (\entityLine submatches ->
            case submatches of
                (Just _) :: _ :: rest ->
                    Succeeded Nothing rest

                Nothing :: rest ->
                    case chompItem entityLine rest of
                        Succeeded value remaining ->
                            Succeeded (Just value) remaining

                        Failed error ->
                            Failed error

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                [ _ ] ->
                    wrongNumberOfSubmatches

                [] ->
                    wrongNumberOfSubmatches
        )


{-| Define a decoder lazily such that it is only constructed if needed. This is
primarily used to break circular reference chains between decoders.

For efficiency reasons, you should make sure that the provided callback function
simply returns an already-existing decoder value; it should _not_ construct
one on the fly.

-}
lazy : (() -> Decoder Entity context a) -> Decoder Entity context a
lazy constructor =
    makeEntityDecoder <|
        \entityLine submatches ->
            let
                (Decoder _ actualChomp) =
                    constructor ()
            in
            actualChomp entityLine submatches
