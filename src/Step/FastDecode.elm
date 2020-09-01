module Step.FastDecode exposing (parse, preprocess)

import Dict exposing (Dict)
import Regex exposing (Regex)
import Step.Types as Types exposing (Attribute, Entity, ParsedAttribute, ParsedEntity)


type alias Database =
    { unparsedEntities : Dict Int UnparsedEntity
    , strings : Dict String String
    }


type alias UnparsedEntity =
    { typeName : String
    , attributeData : String
    }


stringOrCommentRegex : Regex
stringOrCommentRegex =
    Regex.fromString "/\\*[\\s\\S]*?\\*/|'(''|[^'])*'"
        |> Maybe.withDefault Regex.never


entityRegex : Regex
entityRegex =
    Regex.fromStringWith
        { multiline = True
        , caseInsensitive = False
        }
        -- ( id )         (  type name   )       (attributes) --
        "^#(\\d+)\\s*=\\s*([!A-Za-z0-9_]*)\\s*\\(([\\s\\S]*?)\\);$"
        |> Maybe.withDefault Regex.never


whitespaceRegex : Regex
whitespaceRegex =
    Regex.fromStringWith
        { multiline = True
        , caseInsensitive = False
        }
        "\\s+"
        |> Maybe.withDefault Regex.never


stripAll : Regex.Match -> String
stripAll _ =
    ""


stringKey : Int -> String
stringKey number =
    "%" ++ String.fromInt number


stripStringOrComment : Regex.Match -> String
stripStringOrComment { match, number } =
    if String.startsWith "'" match then
        stringKey number

    else
        ""


stringEntry : Regex.Match -> Maybe ( String, String )
stringEntry { match, number } =
    if String.startsWith "'" match then
        Just ( stringKey number, String.slice 1 -1 match )

    else
        Nothing


unparsedEntityEntry : Regex.Match -> Maybe ( Int, UnparsedEntity )
unparsedEntityEntry { submatches } =
    case submatches of
        [ Just idString, Just typeName, Just attributeData ] ->
            case String.toInt idString of
                Just id ->
                    Just
                        ( id
                        , { typeName = String.toUpper typeName
                          , attributeData =
                                attributeData
                                    |> Regex.replace whitespaceRegex stripAll
                          }
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


preprocess : String -> Database
preprocess contents =
    let
        stringOrCommentMatches =
            Regex.find stringOrCommentRegex contents

        strings =
            stringOrCommentMatches
                |> List.filterMap stringEntry
                |> Dict.fromList

        stripped =
            Regex.replace stringOrCommentRegex stripStringOrComment contents

        unparsedEntities =
            Regex.find entityRegex stripped
                |> List.filterMap unparsedEntityEntry
                |> Dict.fromList
    in
    { unparsedEntities = unparsedEntities
    , strings = strings
    }


parse : String -> Result String (List ( Int, ParsedEntity ))
parse contents =
    let
        { unparsedEntities, strings } =
            preprocess contents
    in
    parseEntities strings (Dict.toList unparsedEntities) []


parseEntities : Dict String String -> List ( Int, UnparsedEntity ) -> List ( Int, ParsedEntity ) -> Result String (List ( Int, ParsedEntity ))
parseEntities strings unparsedEntities accumulated =
    case unparsedEntities of
        ( id, unparsedEntity ) :: rest ->
            case parseAttributes strings unparsedEntity.attributeData of
                Ok parsedAttributes ->
                    let
                        parsedEntity =
                            { typeName = Types.TypeName unparsedEntity.typeName
                            , parsedAttributes = parsedAttributes
                            }
                    in
                    parseEntities strings rest (( id, parsedEntity ) :: accumulated)

                Err message ->
                    Err message

        [] ->
            Ok accumulated


attributeTokenRegex : Regex
attributeTokenRegex =
    Regex.fromString "\\(|\\)|[^(),]+"
        |> Maybe.withDefault Regex.never


parseAttributes : Dict String String -> String -> Result String (List ParsedAttribute)
parseAttributes strings attributeData =
    let
        matches =
            Regex.find attributeTokenRegex attributeData
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
                    collectAttributes strings rest (Types.ParsedDefaultAttribute :: accumulated)

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
                                                Types.EnumName (String.slice 1 -1 value)
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
                                                                (Types.ParsedTypedAttribute (Types.TypeName value) rawAttribute :: accumulated)

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
