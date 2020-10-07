module Step.FastParse exposing (Parsed, Preprocessed, parse, postprocess, preprocess)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Parser.Advanced
import Regex exposing (Regex)
import Step.EntityResolution as EntityResolution
import Step.EnumName as EnumName
import Step.File exposing (Header)
import Step.Parse as Parse
import Step.TypeName as TypeName
import Step.Types as Types exposing (Attribute, Entity, File, ParsedAttribute, ParsedEntity)


type alias Preprocessed =
    { original : String
    , unparsedEntities : List ( Int, UnparsedEntity )
    , strings : Dict String String
    }


type alias Parsed =
    { header : Header
    , entities : List ( Int, ParsedEntity )
    }


type UnparsedEntity
    = UnparsedSimpleEntity String String
    | UnparsedComplexEntity String


commentPattern : String
commentPattern =
    -- '/*' followed by anything (including newlines), followed by '*/'
    -- ('\s\S', literally 'space or non-space', seems to be the most robust
    -- way to match anything including newlines)
    "/\\*[\\s\\S]*?\\*/"


stringPattern : String
stringPattern =
    -- single apostrophey followed by some combination of non-apostrophes
    -- or pairs of apostrophes together, followed by a single apostrophe
    "'(''|[^'])*'"


whitespacePattern : String
whitespacePattern =
    "\\s+"


stringOrCommentRegex : Regex
stringOrCommentRegex =
    -- Due to how regexes work, this seems to correctly handle both apostrophes
    -- inside comments and comment begin/end tokens inside strings
    Regex.fromString (commentPattern ++ "|" ++ stringPattern)
        |> Maybe.withDefault Regex.never


stringOrCommentOrWhitespaceRegex : Regex
stringOrCommentOrWhitespaceRegex =
    -- Due to how regexes work, this seems to correctly handle both apostrophes
    -- inside comments and comment begin/end tokens inside strings
    Regex.fromString (commentPattern ++ "|" ++ stringPattern ++ "|" ++ whitespacePattern)
        |> Maybe.withDefault Regex.never


entityRegex : Regex
entityRegex =
    let
        idPattern =
            "#(\\d+)"

        typeNamePattern =
            "([!A-Za-z0-9_]*)"

        attributesPattern =
            "\\(([\\s\\S]*?)\\)"

        entityPattern =
            idPattern ++ "=" ++ typeNamePattern ++ attributesPattern ++ ";"
    in
    Regex.fromStringWith { multiline = True, caseInsensitive = False } entityPattern
        |> Maybe.withDefault Regex.never


stripAll : Regex.Match -> String
stripAll _ =
    ""


stringKey : Int -> String
stringKey number =
    "%" ++ String.fromInt number


stripStringOrCommentOrWhitespace : Regex.Match -> String
stripStringOrCommentOrWhitespace { match, index } =
    if String.startsWith "'" match then
        stringKey index

    else
        ""


unparsedEntityEntry : Regex.Match -> Maybe ( Int, UnparsedEntity )
unparsedEntityEntry { submatches } =
    case submatches of
        [ Just idString, Just typeName, Just attributeData ] ->
            case String.toInt idString of
                Just id ->
                    Just ( id, UnparsedSimpleEntity typeName attributeData )

                Nothing ->
                    Nothing

        [ Just idString, Nothing, Just complexEntityData ] ->
            case String.toInt idString of
                Just id ->
                    Just ( id, UnparsedComplexEntity complexEntityData )

                Nothing ->
                    Nothing

        _ ->
            Nothing


addStringToDict : Regex.Match -> Dict String String -> Dict String String
addStringToDict { match, index } accumulated =
    if String.startsWith "'" match then
        Dict.insert (stringKey index) (String.slice 1 -1 match) accumulated

    else
        accumulated


preprocess : String -> Preprocessed
preprocess contents =
    let
        stringOrCommentMatches =
            Regex.find stringOrCommentRegex contents

        strings =
            List.foldl addStringToDict Dict.empty stringOrCommentMatches

        stripped =
            Regex.replace stringOrCommentOrWhitespaceRegex stripStringOrCommentOrWhitespace contents

        unparsedEntities =
            Regex.find entityRegex stripped
                |> List.filterMap unparsedEntityEntry
    in
    { original = contents
    , unparsedEntities = unparsedEntities
    , strings = strings
    }


parseHeader : String -> Result String Header
parseHeader input =
    let
        parser =
            Parser.succeed identity
                |. Parser.token "ISO-10303-21;"
                |. Parse.whitespace
                |= Parse.header
    in
    Parser.run parser input
        |> Result.mapError (always "Failed to parse header")


postprocess : Preprocessed -> Result String Parsed
postprocess { original, unparsedEntities, strings } =
    Result.map2 Parsed
        (parseHeader original)
        (parseEntities strings unparsedEntities [])


parseEntities : Dict String String -> List ( Int, UnparsedEntity ) -> List ( Int, ParsedEntity ) -> Result String (List ( Int, ParsedEntity ))
parseEntities strings unparsedEntities accumulated =
    case unparsedEntities of
        ( id, unparsedEntity ) :: rest ->
            case unparsedEntity of
                UnparsedSimpleEntity rawTypeName attributeData ->
                    case parseAttributes strings attributeData of
                        Ok parsedAttributes ->
                            let
                                parsedSimpleEntity =
                                    Types.ParsedSimpleEntity (TypeName.fromString rawTypeName)
                                        parsedAttributes
                            in
                            parseEntities strings rest (( id, parsedSimpleEntity ) :: accumulated)

                        Err message ->
                            Err message

                UnparsedComplexEntity complexEntityData ->
                    case parseComplexEntity strings complexEntityData [] of
                        Ok simpleEntities ->
                            let
                                parsedComplexEntity =
                                    Types.ParsedComplexEntity simpleEntities
                            in
                            parseEntities strings rest (( id, parsedComplexEntity ) :: accumulated)

                        Err message ->
                            Err message

        [] ->
            Ok accumulated


entityTokenRegex : Regex
entityTokenRegex =
    Regex.fromString "\\(|\\)|[^(),]+"
        |> Maybe.withDefault Regex.never


parseComplexEntity :
    Dict String String
    -> String
    -> List ( Types.TypeName, List ParsedAttribute )
    -> Result String (List ( Types.TypeName, List ParsedAttribute ))
parseComplexEntity strings complexEntityData accumulated =
    let
        matches =
            Regex.find entityTokenRegex complexEntityData
    in
    collectSimpleEntities strings matches []


collectSimpleEntities : Dict String String -> List Regex.Match -> List ( Types.TypeName, List ParsedAttribute ) -> Result String (List ( Types.TypeName, List ParsedAttribute ))
collectSimpleEntities strings matches accumulated =
    case matches of
        first :: second :: rest ->
            case ( first.match, second.match ) of
                ( typeName, "(" ) ->
                    case collectAttributes strings rest [] of
                        Ok ( parsedAttributes, remaining ) ->
                            collectSimpleEntities strings remaining <|
                                (( TypeName.fromString typeName, parsedAttributes ) :: accumulated)

                        Err message ->
                            Err message

                _ ->
                    Err "Error parsing complex entity"

        [ single ] ->
            Err ("Unexpected termination of complex entity at '" ++ single.match ++ "'")

        [] ->
            Ok (List.reverse accumulated)


parseAttributes : Dict String String -> String -> Result String (List ParsedAttribute)
parseAttributes strings attributeData =
    let
        matches =
            Regex.find entityTokenRegex attributeData
    in
    case collectAttributes strings matches [] of
        Ok ( parsedAttributes, [] ) ->
            Ok parsedAttributes

        Ok ( parsedAttributes, _ ) ->
            Err ("Mismatched parenthesies in '" ++ attributeData ++ "'")

        Err message ->
            Err message


collectAttributes : Dict String String -> List Regex.Match -> List ParsedAttribute -> Result String ( List ParsedAttribute, List Regex.Match )
collectAttributes strings matches accumulated =
    case matches of
        first :: rest ->
            case first.match of
                "(" ->
                    case collectAttributes strings rest [] of
                        Ok ( listEntries, remainingMatches ) ->
                            collectAttributes strings remainingMatches <|
                                (Types.ParsedAttributeList listEntries :: accumulated)

                        Err message ->
                            Err message

                ")" ->
                    Ok ( List.reverse accumulated, rest )

                "$" ->
                    collectAttributes strings rest (Types.ParsedNullAttribute :: accumulated)

                "*" ->
                    collectAttributes strings rest (Types.ParsedDerivedAttribute :: accumulated)

                ".T." ->
                    collectAttributes strings rest (Types.ParsedBoolAttribute True :: accumulated)

                ".F." ->
                    collectAttributes strings rest (Types.ParsedBoolAttribute False :: accumulated)

                value ->
                    case String.toFloat value of
                        Just float ->
                            if String.contains "." value then
                                collectAttributes strings rest <|
                                    (Types.ParsedFloatAttribute float :: accumulated)

                            else
                                collectAttributes strings rest <|
                                    (Types.ParsedIntAttribute (round float) :: accumulated)

                        Nothing ->
                            case String.left 1 value of
                                "#" ->
                                    case String.toInt (String.dropLeft 1 value) of
                                        Just id ->
                                            collectAttributes strings rest <|
                                                (Types.ParsedReference id :: accumulated)

                                        Nothing ->
                                            Err ("Could not parse ID from '" ++ value ++ "'")

                                "%" ->
                                    case Dict.get value strings of
                                        Just string ->
                                            collectAttributes strings rest <|
                                                (Types.ParsedStringAttribute string :: accumulated)

                                        Nothing ->
                                            Err ("Internal error: could not find string with ID " ++ value)

                                "." ->
                                    if String.endsWith "." value then
                                        let
                                            enumName =
                                                EnumName.fromString value
                                        in
                                        collectAttributes strings rest <|
                                            (Types.ParsedEnumAttribute enumName :: accumulated)

                                    else
                                        Err ("Expected enum value '" ++ value ++ "' to end with '.'")

                                "\"" ->
                                    if String.endsWith "\"" value then
                                        let
                                            binaryData =
                                                String.toUpper (String.slice 1 -1 value)
                                        in
                                        collectAttributes strings rest <|
                                            (Types.ParsedBinaryAttribute binaryData :: accumulated)

                                    else
                                        Err ("Expected binary data value '" ++ value ++ "' to end with '\"'")

                                _ ->
                                    case rest of
                                        next :: following ->
                                            case next.match of
                                                "(" ->
                                                    case collectAttributes strings following [] of
                                                        Ok ( [ rawAttribute ], remainingMatches ) ->
                                                            collectAttributes strings remainingMatches <|
                                                                (Types.ParsedTypedAttribute (TypeName.fromString value) rawAttribute :: accumulated)

                                                        Ok ( _, _ ) ->
                                                            Err ("Expected exactly one attribute value for typed attribute of type '" ++ value ++ "'")

                                                        Err message ->
                                                            Err message

                                                _ ->
                                                    Err ("Expected typed attribute of type '" ++ value ++ "' to be followed by an opening parenthesis")

                                        [] ->
                                            Err ("Expected typed attribute of type '" ++ value ++ "' to be followed by an opening parenthesis")

        [] ->
            Ok ( List.reverse accumulated, [] )


parse : String -> Result String Parsed
parse contents =
    contents |> preprocess |> postprocess
