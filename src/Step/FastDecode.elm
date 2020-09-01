module Step.FastDecode exposing (Preprocessed, parse, preprocess)

import Dict exposing (Dict)
import Regex exposing (Regex)
import Step.Types as Types exposing (Attribute, Entity, ParsedAttribute, ParsedEntity)


type alias Preprocessed =
    { unparsedEntities : List ( Int, UnparsedEntity )
    , strings : Dict String String
    }


type UnparsedEntity
    = UnparsedSimpleEntity String String
    | UnparsedComplexEntity String


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


stripWhitespace : String -> String
stripWhitespace string =
    Regex.replace whitespaceRegex stripAll string


unparsedEntityEntry : Regex.Match -> Maybe ( Int, UnparsedEntity )
unparsedEntityEntry { submatches } =
    case submatches of
        [ Just idString, Just typeName, Just attributeData ] ->
            case String.toInt idString of
                Just id ->
                    Just ( id, UnparsedSimpleEntity typeName (stripWhitespace attributeData) )

                Nothing ->
                    Nothing

        [ Just idString, Nothing, Just complexEntityData ] ->
            case String.toInt idString of
                Just id ->
                    Just ( id, UnparsedComplexEntity (stripWhitespace complexEntityData) )

                Nothing ->
                    Nothing

        _ ->
            Nothing


preprocess : String -> Preprocessed
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
    in
    { unparsedEntities = unparsedEntities
    , strings = strings
    }


parse : Preprocessed -> Result String (List ( Int, ParsedEntity ))
parse { unparsedEntities, strings } =
    parseEntities strings unparsedEntities []


parseEntities : Dict String String -> List ( Int, UnparsedEntity ) -> List ( Int, ParsedEntity ) -> Result String (List ( Int, ParsedEntity ))
parseEntities strings unparsedEntities accumulated =
    case unparsedEntities of
        ( id, unparsedEntity ) :: rest ->
            case unparsedEntity of
                UnparsedSimpleEntity typeName attributeData ->
                    case parseAttributes strings attributeData of
                        Ok parsedAttributes ->
                            let
                                parsedSimpleEntity =
                                    Types.ParsedSimpleEntity (Types.TypeName typeName)
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
                                (( Types.TypeName typeName, parsedAttributes ) :: accumulated)

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
