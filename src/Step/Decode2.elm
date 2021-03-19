module Step.Decode2 exposing
    ( Decoder, File
    , parse, header, single, all, get
    , simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12
    , oneOf
    , complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6
    , subEntity0, subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6
    , keepId, ignoreId
    , keep, ignore
    , bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId, referenceWithId
    , typedAttribute
    , map, validate, resolve, andThen, succeed, fail, lazy
    )

{-|

@docs Decoder, File, Entity, SubEntity, Attribute

@docs parse, header, single, all, get


# Entities

@docs simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12


## Alternatives

@docs oneOf


## Complex entities

@docs complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6

@docs subEntity0, subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6


# Attributes

@docs keepId, ignoreId

@docs keep, ignore

@docs bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId, referenceWithId

@docs typedAttribute


# Working with decoders

@docs map, validate, resolve, andThen, succeed, fail, lazy

-}

import Array exposing (Array)
import Bytes.Decode
import Dict
import Regex exposing (Regex)
import Result
import Step.Bytes
import Step.FastParse2 as FastParse2
import Step.Pattern as Pattern exposing (Pattern)
import Step.String
import Step.Types exposing (Attribute, Entity, Header, SubEntity)


type File
    = File Header (Array String)


{-| A `Decoder` describes how to attempt to decode some input of type `i` (an
entire file, an individual entity, a specific attribute) and produce some output
of type `a`.
-}
type Decoder i a
    = Decoder Pattern (Chomp a)


type alias Chomp a =
    EntityLine -> Submatches -> DecodeResult a Submatches


type alias EntityLine =
    { file : File
    , id : Int
    , string : String
    }


type alias Submatches =
    List (Maybe String)


type alias Error =
    { entityIds : List Int
    , message : String
    }


type DecodeResult a remaining
    = Succeeded a remaining
    | Failed Error
    | UnexpectedType


errorMessage : String -> DecodeResult a remaining
errorMessage message =
    Failed
        { entityIds = []
        , message = message
        }


parse : String -> Result String File
parse contents =
    case FastParse2.split contents of
        Just ( parsedHeader, entityLines ) ->
            Ok (File parsedHeader entityLines)

        Nothing ->
            Err "Failed to parse STEP file header"


internalError : String -> DecodeResult a r
internalError message =
    errorMessage ("Internal error in STEP parsing: " ++ message)


header : File -> Header
header file =
    let
        (File fileHeader _) =
            file
    in
    fileHeader


get : Int -> Decoder Entity a -> File -> Result String a
get id entityDecoder file =
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


all : Decoder Entity a -> File -> Result String (List a)
all entityDecoder file =
    let
        (File _ entities) =
            file
    in
    Array.foldl (decodeAllHelp entityDecoder file) ( 0, Ok [] ) entities
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


single : Decoder Entity a -> File -> Result String a
single entityDecoder file =
    case all entityDecoder file of
        Ok [ result ] ->
            Ok result

        Ok [] ->
            Err "No matching entities found"

        Ok (_ :: _ :: _) ->
            Err "More than one matching entity found"

        Err message ->
            Err message


decodeEntity : Decoder Entity a -> EntityLine -> DecodeResult a ()
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


decodeAllHelp : Decoder Entity a -> File -> String -> ( Int, Result String (List a) ) -> ( Int, Result String (List a) )
decodeAllHelp entityDecoder file entityString foldState =
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


makeEntityDecoder : Chomp a -> Decoder Entity a
makeEntityDecoder chomp =
    Decoder Pattern.startOfInput chomp


simpleEntityDecoder : a -> String -> List Pattern -> Chomp (a -> b) -> Decoder Entity b
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


keepId : Decoder Int ((Int -> a) -> a)
keepId =
    Decoder Pattern.startOfInput <|
        \entityLine submatches ->
            Succeeded ((|>) entityLine.id) submatches


ignoreId : Decoder Int (a -> a)
ignoreId =
    Decoder Pattern.startOfInput <|
        \_ submatches ->
            Succeeded identity submatches


thenChomp : Chomp (b -> c) -> Chomp (a -> b) -> Chomp (a -> c)
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


simpleEntity1 : a -> Decoder Int (a -> b) -> String -> Decoder Attribute (b -> c) -> Decoder Entity c
simpleEntity1 callback idDecoder typeName firstAttributeDecoder =
    let
        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        attributePatterns =
            [ pattern1 ]

        chomp =
            chompId |> thenChomp chomp1
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity2 : a -> Decoder Int (a -> b) -> String -> Decoder Attribute (b -> c) -> Decoder Attribute (c -> d) -> Decoder Entity d
simpleEntity2 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder =
    let
        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstAttributeDecoder

        (Decoder pattern2 chomp2) =
            secondAttributeDecoder

        attributePatterns =
            [ pattern1, pattern2 ]

        chomp =
            chompId |> thenChomp chomp1 |> thenChomp chomp2
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity3 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Entity e
simpleEntity3 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder =
    let
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
            chompId |> thenChomp chomp1 |> thenChomp chomp2 |> thenChomp chomp3
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity4 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Entity f
simpleEntity4 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder =
    let
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
            chompId |> thenChomp chomp1 |> thenChomp chomp2 |> thenChomp chomp3 |> thenChomp chomp4
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity5 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Entity g
simpleEntity5 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder =
    let
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
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity6 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Entity h
simpleEntity6 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder =
    let
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
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity7 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Entity i
simpleEntity7 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder =
    let
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
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
                |> thenChomp chomp7
    in
    simpleEntityDecoder callback typeName attributePatterns chomp


simpleEntity8 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Entity j
simpleEntity8 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder =
    let
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
            chompId
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


simpleEntity9 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Attribute (j -> k)
    -> Decoder Entity k
simpleEntity9 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder =
    let
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
            chompId
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


simpleEntity10 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Attribute (j -> k)
    -> Decoder Attribute (k -> l)
    -> Decoder Entity l
simpleEntity10 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder =
    let
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
            chompId
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


simpleEntity11 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Attribute (j -> k)
    -> Decoder Attribute (k -> l)
    -> Decoder Attribute (l -> m)
    -> Decoder Entity m
simpleEntity11 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder =
    let
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
            chompId
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


simpleEntity12 :
    a
    -> Decoder Int (a -> b)
    -> String
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Attribute (j -> k)
    -> Decoder Attribute (k -> l)
    -> Decoder Attribute (l -> m)
    -> Decoder Attribute (m -> n)
    -> Decoder Entity n
simpleEntity12 callback idDecoder typeName firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder =
    let
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
            chompId
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


subEntityDecoder : String -> List Pattern -> Chomp (a -> b) -> Decoder SubEntity (a -> b)
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


subEntity0 : String -> Decoder SubEntity (a -> a)
subEntity0 typeName =
    subEntityDecoder typeName [] (\_ submatches -> Succeeded identity submatches)


subEntity1 : String -> Decoder Attribute (a -> b) -> Decoder SubEntity (a -> b)
subEntity1 typeName firstAttributeDecoder =
    let
        (Decoder pattern1 chomp1) =
            firstAttributeDecoder
    in
    subEntityDecoder typeName [ pattern1 ] chomp1


subEntity2 : String -> Decoder Attribute (a -> b) -> Decoder Attribute (b -> c) -> Decoder SubEntity (a -> c)
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


subEntity3 :
    String
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder SubEntity (a -> d)
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


subEntity4 :
    String
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder SubEntity (a -> e)
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


subEntity5 :
    String
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder SubEntity (a -> f)
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


subEntity6 :
    String
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder SubEntity (a -> g)
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


complexEntityDecoder : a -> List Pattern -> Chomp (a -> b) -> Decoder Entity b
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


complexEntity1 : a -> Decoder Int (a -> b) -> Decoder SubEntity (b -> c) -> Decoder Entity c
complexEntity1 callback idDecoder firstSubEntityDecoder =
    let
        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder
    in
    complexEntityDecoder callback [ pattern1 ] (chompId |> thenChomp chomp1)


complexEntity2 : a -> Decoder Int (a -> b) -> Decoder SubEntity (b -> c) -> Decoder SubEntity (c -> d) -> Decoder Entity d
complexEntity2 callback idDecoder firstSubEntityDecoder secondSubEntityDecoder =
    let
        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        chomp =
            chompId |> thenChomp chomp1 |> thenChomp chomp2
    in
    complexEntityDecoder callback [ pattern1, pattern2 ] chomp


complexEntity3 :
    a
    -> Decoder Int (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder Entity e
complexEntity3 callback idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder =
    let
        (Decoder _ chompId) =
            idDecoder

        (Decoder pattern1 chomp1) =
            firstSubEntityDecoder

        (Decoder pattern2 chomp2) =
            secondSubEntityDecoder

        (Decoder pattern3 chomp3) =
            thirdSubEntityDecoder

        chomp =
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3 ]
        chomp


complexEntity4 :
    a
    -> Decoder Int (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder SubEntity (e -> f)
    -> Decoder Entity f
complexEntity4 callback idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder =
    let
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
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4 ]
        chomp


complexEntity5 :
    a
    -> Decoder Int (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder SubEntity (e -> f)
    -> Decoder SubEntity (f -> g)
    -> Decoder Entity g
complexEntity5 callback idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder =
    let
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
            chompId
                |> thenChomp chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    complexEntityDecoder callback
        [ pattern1, pattern2, pattern3, pattern4, pattern5 ]
        chomp


complexEntity6 :
    a
    -> Decoder Int (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder SubEntity (e -> f)
    -> Decoder SubEntity (f -> g)
    -> Decoder SubEntity (g -> h)
    -> Decoder Entity h
complexEntity6 callback idDecoder firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder =
    let
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
            chompId
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


couldNotParseAttributeAs : String -> DecodeResult a Submatches
couldNotParseAttributeAs dataType =
    errorMessage ("Could not parse attribute as " ++ dataType)


bool : Decoder Attribute Bool
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


string : Decoder Attribute String
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


emptyString : Decoder Attribute ()
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


binaryData : Bytes.Decode.Decoder a -> Decoder Attribute a
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


float : Decoder Attribute Float
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


int : Decoder Attribute Int
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


unexpectedTypeFromAttribute : DecodeResult a r
unexpectedTypeFromAttribute =
    internalError "attribute decoder returned UnexpectedType"


keep : Decoder Attribute a -> Decoder Attribute ((a -> b) -> b)
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


ignore : Decoder Attribute a -> Decoder Attribute (b -> b)
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


list : Decoder Attribute a -> Decoder Attribute (List a)
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


listHelp : Chomp a -> EntityLine -> List Regex.Match -> List a -> List (Maybe String) -> DecodeResult (List a) Submatches
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


tuple2 : Decoder Attribute a -> Decoder Attribute ( a, a )
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


tuple3 : Decoder Attribute a -> Decoder Attribute ( a, a, a )
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


referencedId : Decoder Attribute Int
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


referenceTo : Decoder Entity a -> Decoder Attribute a
referenceTo entityDecoder =
    referenceWithId dropId entityDecoder


referenceWithId : (Int -> a -> b) -> Decoder Entity a -> Decoder Attribute b
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


enum : List ( String, a ) -> Decoder Attribute a
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


typedAttribute : String -> Decoder Attribute a -> Decoder Attribute a
typedAttribute typeName (Decoder pattern chomp) =
    Decoder (Pattern.typedAttribute typeName pattern) chomp


map : (a -> b) -> Decoder i a -> Decoder i b
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


andThen : (a -> Decoder Entity b) -> Decoder Entity a -> Decoder Entity b
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


succeed : a -> Decoder Entity a
succeed value =
    makeEntityDecoder (\_ _ -> Succeeded value [])


fail : String -> Decoder Entity a
fail message =
    makeEntityDecoder (\_ _ -> errorMessage message)


validate : (a -> Result String b) -> Decoder i a -> Decoder i b
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


resolve : Decoder i (Result String a) -> Decoder i a
resolve decoder =
    validate identity decoder


oneOf : List (Decoder Entity a) -> Decoder Entity a
oneOf entityDecoders =
    makeEntityDecoder (chompOneOf entityDecoders)


chompOneOf : List (Decoder Entity a) -> EntityLine -> Submatches -> DecodeResult a Submatches
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


null : a -> Decoder Attribute a
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


derivedValue : a -> Decoder Attribute a
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


optional : Decoder Attribute a -> Decoder Attribute (Maybe a)
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


lazy : (() -> Decoder Entity a) -> Decoder Entity a
lazy constructor =
    makeEntityDecoder <|
        \entityLine submatches ->
            let
                (Decoder _ actualChomp) =
                    constructor ()
            in
            actualChomp entityLine submatches