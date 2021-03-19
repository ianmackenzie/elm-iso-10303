module Step.Preprocess exposing (split)

import Array exposing (Array)
import Regex exposing (Regex)
import Step.Pattern as Pattern
import Step.String
import Step.Types exposing (Header)


split : String -> Maybe ( Header, Array String )
split contents =
    let
        whitespaceOrComment =
            Pattern.zeroOrMore (Pattern.oneOf [ Pattern.whitespace, Pattern.comment ])

        headerPattern =
            Pattern.sequence <|
                List.intersperse whitespaceOrComment <|
                    [ Pattern.startOfInput
                    , Pattern.token "ISO-10303-21;"
                    , Pattern.token "HEADER;"
                    , Pattern.token "FILE_DESCRIPTION"
                    , Pattern.token "("
                    , Pattern.list Pattern.string -- description
                    , Pattern.token ","
                    , Pattern.string -- implementation level
                    , Pattern.token ")"
                    , Pattern.token ";"
                    , Pattern.token "FILE_NAME"
                    , Pattern.token "("
                    , Pattern.string -- file name
                    , Pattern.token ","
                    , Pattern.string -- time stamp
                    , Pattern.token ","
                    , Pattern.list Pattern.string -- author
                    , Pattern.token ","
                    , Pattern.list Pattern.string -- organization
                    , Pattern.token ","
                    , Pattern.string -- preprocessor version
                    , Pattern.token ","
                    , Pattern.string -- originating system
                    , Pattern.token ","
                    , Pattern.string -- authorization
                    , Pattern.token ")"
                    , Pattern.token ";"
                    , Pattern.token "FILE_SCHEMA"
                    , Pattern.token "("
                    , Pattern.list Pattern.string -- schema identifiers
                    , Pattern.token ")"
                    , Pattern.token ";"
                    , Pattern.token "ENDSEC;"
                    , Pattern.token "DATA;"
                    ]

        headerRegex =
            Pattern.compile headerPattern

        stringRegex =
            Pattern.compile Pattern.string

        decode rawString =
            Step.String.decode (String.slice 1 -1 rawString)

        decodeList rawString =
            Regex.find stringRegex rawString |> List.map (.match >> decode)
    in
    case Regex.find headerRegex contents of
        [ { match, submatches } ] ->
            case submatches of
                [ Just description, Just implementationLevel, Just fileName, Just timeStamp, Just author, Just organization, Just preprocessorVersion, Just originatingSystem, Just authorization, Just schemaIdentifiers ] ->
                    Just
                        ( { description = decodeList description
                          , implementationLevel = decode implementationLevel
                          , fileName = decode fileName
                          , timeStamp = decode timeStamp
                          , author = decodeList author
                          , organization = decodeList organization
                          , preprocessorVersion = decode preprocessorVersion
                          , originatingSystem = decode originatingSystem
                          , authorization = decode authorization
                          , schemaIdentifiers = decodeList schemaIdentifiers
                          }
                        , entities (String.dropLeft (String.length match) contents)
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


entities : String -> Array String
entities contents =
    let
        ( pairs, maxId ) =
            collectUnparsedEntities (Regex.find rawEntityRegex contents) [] 0

        sortedPairs =
            List.sortBy (Tuple.first >> negate) pairs

        filled =
            fillGaps maxId sortedPairs []
    in
    Array.fromList filled


collectUnparsedEntities : List Regex.Match -> List ( Int, String ) -> Int -> ( List ( Int, String ), Int )
collectUnparsedEntities regexMatches accumulated maxId =
    case regexMatches of
        first :: rest ->
            case first.submatches of
                [ Just idString, Just data ] ->
                    case String.toInt idString of
                        Just id ->
                            collectUnparsedEntities rest
                                (( id, data ) :: accumulated)
                                (max id maxId)

                        Nothing ->
                            collectUnparsedEntities rest accumulated maxId

                _ ->
                    collectUnparsedEntities rest accumulated maxId

        [] ->
            ( accumulated, maxId )


fillGaps : Int -> List ( Int, String ) -> List String -> List String
fillGaps index pairs accumulated =
    case pairs of
        ( id, data ) :: rest ->
            if id == index then
                fillGaps (index - 1) rest (data :: accumulated)

            else
                fillGaps (index - 1) pairs ("" :: accumulated)

        [] ->
            if index >= 0 then
                fillGaps (index - 1) pairs ("" :: accumulated)

            else
                accumulated


rawEntityRegex : Regex
rawEntityRegex =
    Regex.fromStringWith { multiline = True, caseInsensitive = False } simpleEntityPattern
        |> Maybe.withDefault Regex.never


simpleEntityPattern : String
simpleEntityPattern =
    entityStartPattern ++ attributeChunksPattern ++ entityEndPattern


entityStartPattern : String
entityStartPattern =
    -- Start capture at start of entity type name (if any)
    "#(\\d+)\\s*=\\s*(\\w*\\s*\\("


attributeChunksPattern : String
attributeChunksPattern =
    "(?:(?:" ++ stringChunkPattern ++ ")|(?:" ++ nonStringChunkPattern ++ "))*"


stringChunkPattern : String
stringChunkPattern =
    "'[^']*'"


nonStringChunkPattern : String
nonStringChunkPattern =
    "[^;]+"


entityEndPattern : String
entityEndPattern =
    -- End capture just after final parenthesis
    "\\))\\s*;"
