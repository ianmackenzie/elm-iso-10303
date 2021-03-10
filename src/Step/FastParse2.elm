module Step.FastParse2 exposing (preprocess)

import Array exposing (Array)
import Regex exposing (Regex)


preprocess : String -> Array String
preprocess contents =
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
