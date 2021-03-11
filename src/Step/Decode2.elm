module Step.Decode2 exposing
    ( Decoder, File, Entity, SubEntity, Attribute
    , parse, single, all
    , simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12
    , oneOf
    , complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6
    , subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6
    , keep, ignore
    , bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId
    , map, validate, lazy
    )

{-|

@docs Decoder, File, Entity, SubEntity, Attribute

@docs parse, single, all


# Entities

@docs simpleEntity1, simpleEntity2, simpleEntity3, simpleEntity4, simpleEntity5, simpleEntity6, simpleEntity7, simpleEntity8, simpleEntity9, simpleEntity10, simpleEntity11, simpleEntity12


## Alternatives

@docs oneOf


## Complex entities

@docs complexEntity1, complexEntity2, complexEntity3, complexEntity4, complexEntity5, complexEntity6

@docs subEntity1, subEntity2, subEntity3, subEntity4, subEntity5, subEntity6


# Attributes

@docs keep, ignore

@docs bool, int, float, string, emptyString, binaryData, derivedValue, null, optional, enum, list, tuple2, tuple3, referenceTo, referencedId


# Working with decoders

@docs map, validate, lazy

-}

import Array exposing (Array)
import Bytes.Decode
import Dict
import Regex exposing (Regex)
import Result
import Step.Bytes
import Step.FastParse2 as FastParse2
import Step.String


type File
    = File (Array String)


{-| A `Decoder` describes how to attempt to decode some input of type `i` (an
entire file, an individual entity, a specific attribute) and produce some output
of type `a`.
-}
type Decoder i a
    = Decoder i (Chomp a)


type alias Chomp a =
    File -> String -> Submatches -> DecodeResult a Submatches


type alias Submatches =
    List (Maybe String)


type Entity
    = EntityDecoder


type SubEntity
    = SubEntityDecoder String


type Attribute
    = AttributeDecoder
        { capturingPattern : String
        , nonCapturingPattern : String
        }


type DecodeResult a remaining
    = Succeeded a remaining
    | Failed String
    | UnexpectedType


parse : String -> Result String File
parse contents =
    Ok (File (FastParse2.entities contents))


internalError : String -> DecodeResult a r
internalError message =
    Failed ("Internal error in STEP parsing: " ++ message)


all : Decoder Entity a -> File -> Result String (List a)
all entityDecoder file =
    let
        (File entities) =
            file
    in
    Array.foldl (decodeAllHelp entityDecoder file) (Ok []) entities
        |> Result.map List.reverse


single : Decoder Entity a -> File -> Result String a
single entityDecoder file =
    case all entityDecoder file of
        Ok [ result ] ->
            Ok result

        Ok [] ->
            Err "No matching entities found"

        Ok (_ :: _) ->
            Err "More than one matching entity found"

        Err message ->
            Err message


decodeEntity : Decoder Entity a -> File -> String -> DecodeResult a ()
decodeEntity (Decoder EntityDecoder chomp) file entityString =
    case chomp file entityString [] of
        Succeeded value [] ->
            Succeeded value ()

        Succeeded _ (_ :: _) ->
            wrongNumberOfSubmatches

        Failed message ->
            Failed message

        UnexpectedType ->
            UnexpectedType


decodeAllHelp : Decoder Entity a -> File -> String -> Result String (List a) -> Result String (List a)
decodeAllHelp entityDecoder file entityString currentResult =
    case currentResult of
        Ok accumulated ->
            case decodeEntity entityDecoder file entityString of
                UnexpectedType ->
                    currentResult

                Succeeded value () ->
                    Ok (value :: accumulated)

                Failed message ->
                    Err message

        Err message ->
            Err message


railwayPattern : String -> List String -> String
railwayPattern separator patterns =
    String.join separator (List.map token patterns)


token : String -> String
token pattern =
    String.concat
        [ "(?:"
        , "(?:"
        , pattern
        , ")"
        , "|"
        , "(?:.*)"
        , ")"
        ]


simpleEntityPattern : String -> List String -> String
simpleEntityPattern typeName attributePatterns =
    let
        attributesPattern =
            railwayPattern "\\s*,\\s*" attributePatterns
    in
    String.concat
        [ "^"
        , typeName
        , "\\s*"
        , "\\("
        , "\\s*"
        , "(?:"
        , attributesPattern
        , ")?"
        , "\\s*"
        , "\\)"
        ]


compile : String -> Maybe Regex
compile pattern =
    Regex.fromStringWith { multiline = False, caseInsensitive = True } pattern


simpleEntityRegex : String -> List String -> Result String Regex
simpleEntityRegex typeName attributePatterns =
    let
        pattern =
            simpleEntityPattern typeName attributePatterns
    in
    compile pattern |> Result.fromMaybe pattern


wrongNumberOfSubmatches : DecodeResult a r
wrongNumberOfSubmatches =
    internalError "wrong number of submatches"


dummyChomp : DecodeResult Never r -> Chomp a
dummyChomp decodeResult _ _ _ =
    case decodeResult of
        Succeeded value _ ->
            Succeeded (never value) (never value)

        Failed message ->
            Failed message

        UnexpectedType ->
            UnexpectedType


badEntityDecoder : DecodeResult Never Submatches -> Decoder Entity a
badEntityDecoder result =
    Decoder EntityDecoder (dummyChomp result)


validTypeNameRegex : Regex
validTypeNameRegex =
    Regex.fromString "^[!A-Za-z0-9_]+$" |> Maybe.withDefault Regex.never


isValidTypeName : String -> Bool
isValidTypeName givenTypeName =
    Regex.contains validTypeNameRegex givenTypeName


badTypeName : String -> DecodeResult a r
badTypeName typeName =
    Failed ("Invalid STEP type name '" ++ typeName ++ "'")


simpleEntityDecoder : String -> a -> List String -> Chomp (a -> b) -> Decoder Entity b
simpleEntityDecoder typeName callback attributePatterns chompAttributes =
    if isValidTypeName typeName then
        case simpleEntityRegex typeName attributePatterns of
            Ok regex ->
                Decoder EntityDecoder <|
                    \file entityString _ ->
                        case Regex.find regex entityString of
                            [] ->
                                UnexpectedType

                            [ { submatches } ] ->
                                case chompAttributes file entityString submatches of
                                    Succeeded handler remaining ->
                                        Succeeded (handler callback) remaining

                                    Failed message ->
                                        Failed message

                                    UnexpectedType ->
                                        unexpectedTypeFromAttribute

                            _ :: _ :: _ ->
                                internalError "more than one regex match for a single entity"

            Err badPattern ->
                badEntityDecoder (badRegex badPattern)

    else
        badEntityDecoder (badTypeName typeName)


simpleEntity1 : String -> a -> Decoder Attribute (a -> b) -> Decoder Entity b
simpleEntity1 typeName callback attributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chompAttribute) =
            attributeDecoder
    in
    simpleEntityDecoder typeName callback [ decoder1.capturingPattern ] chompAttribute


thenChomp : Chomp (b -> c) -> Chomp (a -> b) -> Chomp (a -> c)
thenChomp chomp2 chomp1 =
    \file entityString submatches ->
        case chomp1 file entityString submatches of
            Succeeded result1 after1 ->
                case chomp2 file entityString after1 of
                    Succeeded result2 after2 ->
                        Succeeded (result1 >> result2) after2

                    Failed message ->
                        Failed message

                    UnexpectedType ->
                        UnexpectedType

            Failed message ->
                Failed message

            UnexpectedType ->
                UnexpectedType


simpleEntity2 : String -> a -> Decoder Attribute (a -> b) -> Decoder Attribute (b -> c) -> Decoder Entity c
simpleEntity2 typeName callback firstAttributeDecoder secondAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern, decoder2.capturingPattern ]

        chompAttributes =
            chomp1 |> thenChomp chomp2
    in
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity3 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Entity d
simpleEntity3 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern, decoder2.capturingPattern, decoder3.capturingPattern ]

        chompAttributes =
            chomp1 |> thenChomp chomp2 |> thenChomp chomp3
    in
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity4 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Entity e
simpleEntity4 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            ]

        chompAttributes =
            chomp1 |> thenChomp chomp2 |> thenChomp chomp3 |> thenChomp chomp4
    in
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity5 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Entity f
simpleEntity5 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity6 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Entity g
simpleEntity6 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            ]

        chompAttributes =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity7 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Entity h
simpleEntity7 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity8 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Entity i
simpleEntity8 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        (Decoder (AttributeDecoder decoder8) chomp8) =
            eighthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
            , decoder8.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity9 :
    String
    -> a
    -> Decoder Attribute (a -> b)
    -> Decoder Attribute (b -> c)
    -> Decoder Attribute (c -> d)
    -> Decoder Attribute (d -> e)
    -> Decoder Attribute (e -> f)
    -> Decoder Attribute (f -> g)
    -> Decoder Attribute (g -> h)
    -> Decoder Attribute (h -> i)
    -> Decoder Attribute (i -> j)
    -> Decoder Entity j
simpleEntity9 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        (Decoder (AttributeDecoder decoder8) chomp8) =
            eighthAttributeDecoder

        (Decoder (AttributeDecoder decoder9) chomp9) =
            ninthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
            , decoder8.capturingPattern
            , decoder9.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity10 :
    String
    -> a
    -> Decoder Attribute (a -> b)
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
simpleEntity10 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        (Decoder (AttributeDecoder decoder8) chomp8) =
            eighthAttributeDecoder

        (Decoder (AttributeDecoder decoder9) chomp9) =
            ninthAttributeDecoder

        (Decoder (AttributeDecoder decoder10) chomp10) =
            tenthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
            , decoder8.capturingPattern
            , decoder9.capturingPattern
            , decoder10.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity11 :
    String
    -> a
    -> Decoder Attribute (a -> b)
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
simpleEntity11 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        (Decoder (AttributeDecoder decoder8) chomp8) =
            eighthAttributeDecoder

        (Decoder (AttributeDecoder decoder9) chomp9) =
            ninthAttributeDecoder

        (Decoder (AttributeDecoder decoder10) chomp10) =
            tenthAttributeDecoder

        (Decoder (AttributeDecoder decoder11) chomp11) =
            eleventhAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
            , decoder8.capturingPattern
            , decoder9.capturingPattern
            , decoder10.capturingPattern
            , decoder11.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


simpleEntity12 :
    String
    -> a
    -> Decoder Attribute (a -> b)
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
simpleEntity12 typeName callback firstAttributeDecoder secondAttributeDecoder thirdAttributeDecoder fourthAttributeDecoder fifthAttributeDecoder sixthAttributeDecoder seventhAttributeDecoder eighthAttributeDecoder ninthAttributeDecoder tenthAttributeDecoder eleventhAttributeDecoder twelfthAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        (Decoder (AttributeDecoder decoder7) chomp7) =
            seventhAttributeDecoder

        (Decoder (AttributeDecoder decoder8) chomp8) =
            eighthAttributeDecoder

        (Decoder (AttributeDecoder decoder9) chomp9) =
            ninthAttributeDecoder

        (Decoder (AttributeDecoder decoder10) chomp10) =
            tenthAttributeDecoder

        (Decoder (AttributeDecoder decoder11) chomp11) =
            eleventhAttributeDecoder

        (Decoder (AttributeDecoder decoder12) chomp12) =
            twelfthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
            , decoder7.capturingPattern
            , decoder8.capturingPattern
            , decoder9.capturingPattern
            , decoder10.capturingPattern
            , decoder11.capturingPattern
            , decoder12.capturingPattern
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
    simpleEntityDecoder typeName callback attributePatterns chompAttributes


badSubEntityDecoder : DecodeResult Never Submatches -> Decoder SubEntity a
badSubEntityDecoder result =
    Decoder (SubEntityDecoder "") (dummyChomp result)


subEntityDecoder : String -> List String -> Chomp (a -> b) -> Decoder SubEntity (a -> b)
subEntityDecoder typeName attributePatterns chompAttributes =
    if isValidTypeName typeName then
        let
            pattern =
                String.concat
                    [ "("
                    , typeName
                    , ")"
                    , "\\s*"
                    , "\\("
                    , "\\s*"
                    , "(?:"
                    , railwayPattern "\\s*,\\s*" attributePatterns
                    , ")?"
                    , "\\s*"
                    , "\\)"
                    ]
        in
        Decoder
            (SubEntityDecoder pattern)
            (\file entityString submatches ->
                case submatches of
                    -- Type name was matched, proceed to chomp attributes
                    (Just _) :: rest ->
                        case chompAttributes file entityString rest of
                            Succeeded handler remaining ->
                                Succeeded handler remaining

                            Failed message ->
                                Failed message

                            UnexpectedType ->
                                unexpectedTypeFromAttribute

                    -- Type name was not matched
                    Nothing :: _ ->
                        UnexpectedType

                    [] ->
                        wrongNumberOfSubmatches
            )

    else
        badSubEntityDecoder (badTypeName typeName)


subEntity1 : String -> Decoder Attribute (a -> b) -> Decoder SubEntity (a -> b)
subEntity1 typeName attributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chompAttribute) =
            attributeDecoder
    in
    subEntityDecoder typeName [ decoder1.capturingPattern ] chompAttribute


subEntity2 : String -> Decoder Attribute (a -> b) -> Decoder Attribute (b -> c) -> Decoder SubEntity (a -> c)
subEntity2 typeName firstAttributeDecoder secondAttributeDecoder =
    let
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern, decoder2.capturingPattern ]

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
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
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
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
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
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
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
        (Decoder (AttributeDecoder decoder1) chomp1) =
            firstAttributeDecoder

        (Decoder (AttributeDecoder decoder2) chomp2) =
            secondAttributeDecoder

        (Decoder (AttributeDecoder decoder3) chomp3) =
            thirdAttributeDecoder

        (Decoder (AttributeDecoder decoder4) chomp4) =
            fourthAttributeDecoder

        (Decoder (AttributeDecoder decoder5) chomp5) =
            fifthAttributeDecoder

        (Decoder (AttributeDecoder decoder6) chomp6) =
            sixthAttributeDecoder

        attributePatterns =
            [ decoder1.capturingPattern
            , decoder2.capturingPattern
            , decoder3.capturingPattern
            , decoder4.capturingPattern
            , decoder5.capturingPattern
            , decoder6.capturingPattern
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


complexEntityPattern : List SubEntity -> String
complexEntityPattern subEntities =
    let
        subEntityPatterns =
            List.map (\(SubEntityDecoder pattern) -> pattern) subEntities
    in
    String.concat
        [ "^"
        , "\\("
        , "\\s*"
        , railwayPattern "\\s*" subEntityPatterns
        , "\\s*"
        , "\\)"
        ]


complexEntityRegex : List SubEntity -> Result String Regex
complexEntityRegex subEntities =
    let
        pattern =
            complexEntityPattern subEntities
    in
    compile pattern |> Result.fromMaybe pattern


complexEntityDecoder : a -> List SubEntity -> Chomp (a -> b) -> Decoder Entity b
complexEntityDecoder callback subEntities chomp =
    case complexEntityRegex subEntities of
        Ok regex ->
            Decoder EntityDecoder <|
                \file entityString _ ->
                    case Regex.find regex entityString of
                        [] ->
                            UnexpectedType

                        [ { submatches } ] ->
                            case chomp file entityString submatches of
                                Succeeded handler remaining ->
                                    Succeeded (handler callback) remaining

                                Failed message ->
                                    Failed message

                                UnexpectedType ->
                                    UnexpectedType

                        _ :: _ :: _ ->
                            internalError "more than one regex match for a single entity"

        Err badPattern ->
            badEntityDecoder (badRegex badPattern)


complexEntity1 : a -> Decoder SubEntity (a -> b) -> Decoder Entity b
complexEntity1 callback firstSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder
    in
    complexEntityDecoder callback [ decoder1 ] chomp1


complexEntity2 : a -> Decoder SubEntity (a -> b) -> Decoder SubEntity (b -> c) -> Decoder Entity c
complexEntity2 callback firstSubEntityDecoder secondSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder

        (Decoder decoder2 chomp2) =
            secondSubEntityDecoder

        chomp =
            chomp1 |> thenChomp chomp2
    in
    complexEntityDecoder callback [ decoder1, decoder2 ] chomp


complexEntity3 :
    a
    -> Decoder SubEntity (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder Entity d
complexEntity3 callback firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder

        (Decoder decoder2 chomp2) =
            secondSubEntityDecoder

        (Decoder decoder3 chomp3) =
            thirdSubEntityDecoder

        chomp =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
    in
    complexEntityDecoder callback
        [ decoder1, decoder2, decoder3 ]
        chomp


complexEntity4 :
    a
    -> Decoder SubEntity (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder Entity e
complexEntity4 callback firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder

        (Decoder decoder2 chomp2) =
            secondSubEntityDecoder

        (Decoder decoder3 chomp3) =
            thirdSubEntityDecoder

        (Decoder decoder4 chomp4) =
            fourthSubEntityDecoder

        chomp =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
    in
    complexEntityDecoder callback
        [ decoder1, decoder2, decoder3, decoder4 ]
        chomp


complexEntity5 :
    a
    -> Decoder SubEntity (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder SubEntity (e -> f)
    -> Decoder Entity f
complexEntity5 callback firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder

        (Decoder decoder2 chomp2) =
            secondSubEntityDecoder

        (Decoder decoder3 chomp3) =
            thirdSubEntityDecoder

        (Decoder decoder4 chomp4) =
            fourthSubEntityDecoder

        (Decoder decoder5 chomp5) =
            fifthSubEntityDecoder

        chomp =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
    in
    complexEntityDecoder callback
        [ decoder1, decoder2, decoder3, decoder4, decoder5 ]
        chomp


complexEntity6 :
    a
    -> Decoder SubEntity (a -> b)
    -> Decoder SubEntity (b -> c)
    -> Decoder SubEntity (c -> d)
    -> Decoder SubEntity (d -> e)
    -> Decoder SubEntity (e -> f)
    -> Decoder SubEntity (f -> g)
    -> Decoder Entity g
complexEntity6 callback firstSubEntityDecoder secondSubEntityDecoder thirdSubEntityDecoder fourthSubEntityDecoder fifthSubEntityDecoder sixthSubEntityDecoder =
    let
        (Decoder decoder1 chomp1) =
            firstSubEntityDecoder

        (Decoder decoder2 chomp2) =
            secondSubEntityDecoder

        (Decoder decoder3 chomp3) =
            thirdSubEntityDecoder

        (Decoder decoder4 chomp4) =
            fourthSubEntityDecoder

        (Decoder decoder5 chomp5) =
            fifthSubEntityDecoder

        (Decoder decoder6 chomp6) =
            sixthSubEntityDecoder

        chomp =
            chomp1
                |> thenChomp chomp2
                |> thenChomp chomp3
                |> thenChomp chomp4
                |> thenChomp chomp5
                |> thenChomp chomp6
    in
    complexEntityDecoder callback
        [ decoder1, decoder2, decoder3, decoder4, decoder5, decoder6 ]
        chomp


couldNotParseAttributeAs : String -> DecodeResult a Submatches
couldNotParseAttributeAs dataType =
    Failed ("Could not parse attribute as " ++ dataType)


badAttributeDecoder : DecodeResult Never Submatches -> Decoder Attribute a
badAttributeDecoder result =
    Decoder (AttributeDecoder { capturingPattern = "", nonCapturingPattern = "" })
        (dummyChomp result)


bool : Decoder Attribute Bool
bool =
    Decoder
        (AttributeDecoder
            { capturingPattern = "\\.([TF])\\."
            , nonCapturingPattern = "\\.[TF]\\."
            }
        )
        (\_ _ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    Succeeded (submatch == "T") rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "bool"

                [] ->
                    wrongNumberOfSubmatches
        )


stringPatterns : { capturingPattern : String, nonCapturingPattern : String }
stringPatterns =
    { capturingPattern = "('(?:''|[^'])*')"
    , nonCapturingPattern = "'(?:''|[^'])*'"
    }


string : Decoder Attribute String
string =
    Decoder
        (AttributeDecoder stringPatterns)
        (\_ _ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    Succeeded (Step.String.decode (String.slice 1 -1 submatch)) rest

                Nothing :: _ ->
                    couldNotParseAttributeAs "string"

                [] ->
                    wrongNumberOfSubmatches
        )


emptyString : Decoder Attribute ()
emptyString =
    Decoder (AttributeDecoder stringPatterns) <|
        \_ _ submatches ->
            case submatches of
                (Just "''") :: rest ->
                    Succeeded () rest

                (Just _) :: _ ->
                    Failed "String is not empty"

                Nothing :: _ ->
                    couldNotParseAttributeAs "string"

                [] ->
                    wrongNumberOfSubmatches


binaryData : Bytes.Decode.Decoder a -> Decoder Attribute a
binaryData bytesDecoder =
    Decoder
        (AttributeDecoder
            { nonCapturingPattern = "\"[A-Z0-9]*\""
            , capturingPattern = "\"([A-Z0-9]*)\""
            }
        )
        (\_ _ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case Bytes.Decode.decode bytesDecoder (Step.Bytes.decode submatch) of
                        Just result ->
                            Succeeded result rest

                        Nothing ->
                            Failed "Decoding of binary data failed"

                Nothing :: _ ->
                    couldNotParseAttributeAs "binary data"

                [] ->
                    internalError "could not decode binary attribute"
        )


float : Decoder Attribute Float
float =
    Decoder
        (AttributeDecoder
            { nonCapturingPattern = "[-+]?\\d+\\.\\d*(?:E[-+]?\\d+)?"
            , capturingPattern = "([-+]?\\d+\\.\\d*(?:E[-+]?\\d+)?)"
            }
        )
        (\_ _ submatches ->
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
        )


int : Decoder Attribute Int
int =
    Decoder
        (AttributeDecoder
            { nonCapturingPattern = "[-+]?\\d+"
            , capturingPattern = "([-+]?\\d+)"
            }
        )
        (\_ _ submatches ->
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
        )


unexpectedTypeFromAttribute : DecodeResult a r
unexpectedTypeFromAttribute =
    internalError "attribute decoder returned UnexpectedType"


badRegex : String -> DecodeResult a r
badRegex pattern =
    internalError ("bad regex '" ++ pattern ++ "'")


keep : Decoder Attribute a -> Decoder Attribute ((a -> b) -> b)
keep attributeDecoder =
    let
        (Decoder decoder chompAttribute) =
            attributeDecoder
    in
    Decoder decoder
        (\file entityString submatches ->
            case chompAttribute file entityString submatches of
                Succeeded value remaining ->
                    Succeeded ((|>) value) remaining

                Failed message ->
                    Failed message

                UnexpectedType ->
                    unexpectedTypeFromAttribute
        )


ignore : Decoder Attribute a -> Decoder Attribute (b -> b)
ignore attributeDecoder =
    let
        (Decoder decoder chompAttribute) =
            attributeDecoder
    in
    Decoder decoder
        (\file entityString submatches ->
            case chompAttribute file entityString submatches of
                Succeeded _ remaining ->
                    Succeeded identity remaining

                Failed message ->
                    Failed message

                UnexpectedType ->
                    unexpectedTypeFromAttribute
        )



-- ignore : Decoder Attribute a -> Decoder Attribute (b -> b)
-- ignore attributeDecoder =
--     let
--         (Decoder (AttributeDecoder { nonCapturingPattern }) _) =
--             attributeDecoder
--     in
--     Decoder
--         (AttributeDecoder
--             { nonCapturingPattern = nonCapturingPattern
--             , capturingPattern = nonCapturingPattern
--             }
--         )
--         (\_ submatches -> Succeeded identity submatches)


list : Decoder Attribute a -> Decoder Attribute (List a)
list itemDecoder =
    let
        (Decoder (AttributeDecoder { capturingPattern, nonCapturingPattern }) chompItem) =
            itemDecoder

        terminator =
            ",|(?=\\))"

        repeatingPattern =
            "\\s*" ++ nonCapturingPattern ++ "\\s*" ++ "(?:" ++ terminator ++ ")"

        items =
            "(?:" ++ repeatingPattern ++ ")*"

        nonEmptyListPattern =
            "\\(" ++ items ++ "\\)"

        emptyListPattern =
            "\\(\\s*\\)"

        pattern =
            "(?:" ++ nonEmptyListPattern ++ ")|(?:" ++ emptyListPattern ++ ")"

        searchPattern =
            "\\s*" ++ capturingPattern ++ "\\s*(?:,|$)"
    in
    case compile searchPattern of
        Just searchRegex ->
            Decoder (AttributeDecoder { nonCapturingPattern = pattern, capturingPattern = "(" ++ pattern ++ ")" })
                (\file entityString submatches ->
                    case submatches of
                        (Just listString) :: rest ->
                            listHelp chompItem file entityString (Regex.find searchRegex (String.slice 1 -1 listString)) [] rest

                        Nothing :: _ ->
                            couldNotParseAttributeAs "list"

                        [] ->
                            wrongNumberOfSubmatches
                )

        Nothing ->
            badAttributeDecoder (badRegex searchPattern)


listHelp : Chomp a -> File -> String -> List Regex.Match -> List a -> List (Maybe String) -> DecodeResult (List a) Submatches
listHelp chompItem file entityString matches accumulated remainingSubmatches =
    case matches of
        { submatches } :: rest ->
            case chompItem file entityString submatches of
                Succeeded item [] ->
                    listHelp chompItem file entityString rest (item :: accumulated) remainingSubmatches

                Succeeded _ (_ :: _) ->
                    wrongNumberOfSubmatches

                Failed message ->
                    Failed message

                UnexpectedType ->
                    unexpectedTypeFromAttribute

        [] ->
            Succeeded (List.reverse accumulated) remainingSubmatches


tuplePattern : List String -> String
tuplePattern attributePatterns =
    String.concat
        [ "\\("
        , "\\s*"
        , railwayPattern "\\s*,\\s*" attributePatterns
        , "\\s*"
        , "\\)"
        ]


tuple2 : Decoder Attribute a -> Decoder Attribute ( a, a )
tuple2 itemDecoder =
    let
        (Decoder (AttributeDecoder attribute) chompItem) =
            itemDecoder

        capturingPattern =
            tuplePattern [ attribute.capturingPattern, attribute.capturingPattern ]

        nonCapturingPattern =
            tuplePattern [ attribute.nonCapturingPattern, attribute.nonCapturingPattern ]
    in
    Decoder
        (AttributeDecoder
            { capturingPattern = capturingPattern
            , nonCapturingPattern = nonCapturingPattern
            }
        )
        (\file entityString submatches ->
            case chompItem file entityString submatches of
                Succeeded firstValue afterFirst ->
                    case chompItem file entityString afterFirst of
                        Succeeded secondValue afterSecond ->
                            Succeeded ( firstValue, secondValue ) afterSecond

                        Failed message ->
                            Failed message

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                Failed message ->
                    Failed message

                UnexpectedType ->
                    unexpectedTypeFromAttribute
        )


tuple3 : Decoder Attribute a -> Decoder Attribute ( a, a, a )
tuple3 itemDecoder =
    let
        (Decoder (AttributeDecoder attribute) chompItem) =
            itemDecoder

        capturingPattern =
            tuplePattern [ attribute.capturingPattern, attribute.capturingPattern, attribute.capturingPattern ]

        nonCapturingPattern =
            tuplePattern [ attribute.nonCapturingPattern, attribute.nonCapturingPattern, attribute.nonCapturingPattern ]
    in
    Decoder
        (AttributeDecoder
            { capturingPattern = capturingPattern
            , nonCapturingPattern = nonCapturingPattern
            }
        )
        (\file entityString submatches ->
            case chompItem file entityString submatches of
                Succeeded firstValue afterFirst ->
                    case chompItem file entityString afterFirst of
                        Succeeded secondValue afterSecond ->
                            case chompItem file entityString afterSecond of
                                Succeeded thirdValue afterThird ->
                                    Succeeded ( firstValue, secondValue, thirdValue ) afterThird

                                Failed message ->
                                    Failed message

                                UnexpectedType ->
                                    unexpectedTypeFromAttribute

                        Failed message ->
                            Failed message

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                Failed message ->
                    Failed message

                UnexpectedType ->
                    unexpectedTypeFromAttribute
        )


referencedId : Decoder Attribute Int
referencedId =
    Decoder
        (AttributeDecoder
            { capturingPattern = "#(\\d+)"
            , nonCapturingPattern = "#\\d+"
            }
        )
        (\_ _ submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case String.toInt submatch of
                        Just id ->
                            Succeeded id rest

                        Nothing ->
                            internalError ("failed to parse referenced ID '" ++ submatch ++ "'")

                Nothing :: _ ->
                    couldNotParseAttributeAs "entity reference"

                [] ->
                    wrongNumberOfSubmatches
        )


referenceTo : Decoder Entity a -> Decoder Attribute a
referenceTo entityDecoder =
    Decoder
        (AttributeDecoder
            { capturingPattern = "#(\\d+)"
            , nonCapturingPattern = "#\\d+"
            }
        )
        (\((File entities) as file) parentEntityString submatches ->
            case submatches of
                (Just submatch) :: rest ->
                    case String.toInt submatch of
                        Just id ->
                            case Array.get id entities of
                                Just childEntityString ->
                                    case decodeEntity entityDecoder file childEntityString of
                                        Succeeded value () ->
                                            Succeeded value rest

                                        UnexpectedType ->
                                            Failed ("Referenced entity '" ++ childEntityString ++ "' has unexpected type")

                                        Failed message ->
                                            Failed message

                                Nothing ->
                                    Failed ("No entity with ID " ++ submatch)

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

        capturingPattern =
            "\\.(\\w+)\\."

        nonCapturingPattern =
            "\\.\\w+\\."
    in
    Decoder
        (AttributeDecoder
            { capturingPattern = capturingPattern
            , nonCapturingPattern = nonCapturingPattern
            }
        )
        (\_ _ submatches ->
            case submatches of
                (Just enumString) :: remaining ->
                    case Dict.get enumString lookupDict of
                        Just value ->
                            Succeeded value remaining

                        Nothing ->
                            Failed ("unexpected enum value '" ++ enumString ++ "'")

                Nothing :: _ ->
                    couldNotParseAttributeAs "enum"

                [] ->
                    wrongNumberOfSubmatches
        )


map : (a -> b) -> Decoder i a -> Decoder i b
map function decoder =
    let
        (Decoder impl chomp) =
            decoder
    in
    Decoder impl
        (\file entityString submatches ->
            case chomp file entityString submatches of
                Succeeded value remaining ->
                    Succeeded (function value) remaining

                Failed message ->
                    Failed message

                UnexpectedType ->
                    UnexpectedType
        )


validate : (a -> Result String b) -> Decoder i a -> Decoder i b
validate function decoder =
    let
        (Decoder impl chomp) =
            decoder
    in
    Decoder impl
        (\file entityString submatches ->
            case chomp file entityString submatches of
                Succeeded value remaining ->
                    case function value of
                        Ok result ->
                            Succeeded result remaining

                        Err message ->
                            Failed message

                Failed message ->
                    Failed message

                UnexpectedType ->
                    UnexpectedType
        )


oneOf : List (Decoder Entity a) -> Decoder Entity a
oneOf entityDecoders =
    Decoder EntityDecoder (chompOneOf entityDecoders)


chompOneOf : List (Decoder Entity a) -> File -> String -> Submatches -> DecodeResult a Submatches
chompOneOf entityDecoders file entityString _ =
    case entityDecoders of
        (Decoder EntityDecoder chomp) :: rest ->
            case chomp file entityString [] of
                Succeeded value remaining ->
                    Succeeded value []

                UnexpectedType ->
                    chompOneOf rest file entityString []

                Failed message ->
                    Failed message

        [] ->
            UnexpectedType


null : a -> Decoder Attribute a
null result =
    Decoder
        (AttributeDecoder
            { capturingPattern = "\\(\\$)\\"
            , nonCapturingPattern = "\\$"
            }
        )
        (\_ _ submatches ->
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
    Decoder
        (AttributeDecoder
            { capturingPattern = "\\(\\*)\\"
            , nonCapturingPattern = "\\*"
            }
        )
        (\_ _ submatches ->
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
        (Decoder (AttributeDecoder { capturingPattern, nonCapturingPattern }) chompItem) =
            decoder
    in
    Decoder
        (AttributeDecoder
            { capturingPattern = "(?:" ++ capturingPattern ++ ")|(\\$)"
            , nonCapturingPattern = "(?:" ++ nonCapturingPattern ++ ")|\\$"
            }
        )
        (\file entityString submatches ->
            case submatches of
                (Just itemSubmatch) :: _ :: following ->
                    case chompItem file entityString (Just itemSubmatch :: following) of
                        Succeeded value remaining ->
                            Succeeded (Just value) remaining

                        Failed message ->
                            Failed message

                        UnexpectedType ->
                            unexpectedTypeFromAttribute

                Nothing :: (Just _) :: rest ->
                    Succeeded Nothing rest

                Nothing :: Nothing :: _ ->
                    couldNotParseAttributeAs "optional value"

                [ singleSubmatch ] ->
                    wrongNumberOfSubmatches

                [] ->
                    wrongNumberOfSubmatches
        )


lazy : (() -> Decoder Entity a) -> Decoder Entity a
lazy constructor =
    Decoder EntityDecoder <|
        \file entityString _ ->
            let
                (Decoder EntityDecoder actualChomp) =
                    constructor ()
            in
            actualChomp file entityString []
