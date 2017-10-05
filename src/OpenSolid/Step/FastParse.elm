module OpenSolid.Step.FastParse exposing (..)

{-| Functionality for parsing STEP files.

@docs Error, file

-}

import OpenSolid.Step exposing (Entity, File, Header)
import OpenSolid.Step.EntityResolution as EntityResolution
import OpenSolid.Step.Parse as Parse
import OpenSolid.Step.Types as Types
import Parser exposing ((|.), (|=))
import Parser.LowLevel
import Regex exposing (Regex)


{-| Types of errors that can be encountered when parsing a file:

  - A `SyntaxError` means an error actually parsing STEP text; this means that
    either the STEP file is improperly formatted or (more likely!) it uses
    an aspect of STEP syntax that is not yet supported by this package. The
    parameter is an error string that can be used for debugging (not suitable to
    be shown to end users).
  - A `NonexistentEntity` means that the file was parsed OK, but an error
    occurred when a reference such as `#23` was found in one entity but no
    entity with that ID existed in the file. The integer parameter is the ID of
    the nonexistent entity.
  - A `CircularReference` means that the files was parsed OK, but a circular
    reference was found between entities (this is possible in STEP but not
    currently supported by this package). The parameter is the circular
    reference chain: `[34, 34]` means that entity #34 refers to itself, while
    `[34, 5, 126, 34]` means that entity #34 refers to #5, which refers to #126,
    which refers back to #34.

-}
type Error
    = SyntaxError String
    | NonexistentEntity Int
    | CircularReference (List Int)


find : Regex -> String -> List Regex.Match
find =
    Regex.find (Regex.AtMost 1)


inspect : String -> String
inspect input =
    if String.length input <= 20 then
        "\"" ++ input ++ "\""
    else
        "\"" ++ String.left 17 input ++ "...\""


err : String -> String -> Result String a
err message input =
    Err (message ++ " " ++ inspect input)


stringRegex : Regex
stringRegex =
    Regex.regex "^('[^']*')+"


parseString : String -> Result String ( Types.ParsedAttribute, String )
parseString input =
    case find stringRegex input of
        [ { match } ] ->
            Ok
                ( Types.ParsedStringAttribute (String.slice 1 -1 match)
                , String.dropLeft (String.length match) input
                )

        _ ->
            err "Could not parse string" input


binaryRegex : Regex
binaryRegex =
    Regex.regex "^\"[^\"]*\""


parseBinary : String -> Result String ( Types.ParsedAttribute, String )
parseBinary input =
    case find binaryRegex input of
        [ { match } ] ->
            Ok
                ( Types.ParsedBinaryAttribute (String.slice 1 -1 match)
                , dropMatch match input
                )

        _ ->
            err "Could not parse binary" input


numberRegex : Regex
numberRegex =
    Regex.regex "^[+-]?\\d+(\\.\\d*(E[+-]?\\d+)?)?"


parseNumber : String -> Result String ( Types.ParsedAttribute, String )
parseNumber input =
    case find numberRegex input of
        [ { match, submatches } ] ->
            case submatches of
                [ Nothing, Nothing ] ->
                    case String.toInt match of
                        Ok value ->
                            Ok
                                ( Types.ParsedIntAttribute value
                                , dropMatch match input
                                )

                        Err _ ->
                            err "Could not parse integer" input

                _ ->
                    case String.toFloat match of
                        Ok value ->
                            Ok
                                ( Types.ParsedFloatAttribute value
                                , dropMatch match input
                                )

                        Err _ ->
                            err "Could not parse float" input

        _ ->
            err "Could not parse number" input


enumRegex : Regex
enumRegex =
    Regex.regex "^\\.[A-Z_][A-Z_0-9]*\\."


dropMatch : String -> String -> String
dropMatch match input =
    String.dropLeft (String.length match) input


parseEnum : String -> Result String ( Types.ParsedAttribute, String )
parseEnum input =
    if String.startsWith ".T." input then
        Ok ( Types.ParsedBoolAttribute True, String.dropLeft 3 input )
    else if String.startsWith ".F." input then
        Ok ( Types.ParsedBoolAttribute False, String.dropLeft 3 input )
    else
        case find enumRegex input of
            [ { match } ] ->
                let
                    value =
                        String.slice 1 -1 match

                    parsedAttribute =
                        if value == "T" then
                            Types.ParsedBoolAttribute True
                        else if value == "F" then
                            Types.ParsedBoolAttribute False
                        else
                            Types.ParsedEnumAttribute (Types.EnumName value)
                in
                Ok ( parsedAttribute, dropMatch match input )

            _ ->
                err "Could not parse enum" input


idRegex : Regex
idRegex =
    Regex.regex "#\\d+"


parseReference : String -> Result String ( Types.ParsedAttribute, String )
parseReference input =
    case find idRegex input of
        [ { match } ] ->
            case String.toInt (String.dropLeft 1 match) of
                Ok value ->
                    Ok ( Types.ParsedReference value, dropMatch match input )

                Err _ ->
                    Err ("Could not parse reference " ++ inspect input)

        _ ->
            err "Could not parse reference" input


parseAttributes : String -> Result String ( List Types.ParsedAttribute, String )
parseAttributes input =
    let
        inputPastParenthesis =
            String.dropLeft 1 input
    in
    if String.startsWith ")" inputPastParenthesis then
        Ok ( [], String.dropLeft 1 inputPastParenthesis )
    else
        accumulateAttributes inputPastParenthesis []


accumulateAttributes : String -> List Types.ParsedAttribute -> Result String ( List Types.ParsedAttribute, String )
accumulateAttributes input accumulated =
    case parseAttribute input of
        Ok ( parsedAttribute, remainingInput ) ->
            let
                prepended =
                    parsedAttribute :: accumulated

                followingInput =
                    String.dropLeft 1 remainingInput
            in
            if String.startsWith "," remainingInput then
                accumulateAttributes followingInput prepended
            else if String.startsWith ")" remainingInput then
                Ok
                    ( List.reverse prepended
                    , followingInput
                    )
            else
                err "Could not parse attributes" input

        Err msg ->
            Err msg


parseAttributeList : String -> Result String ( Types.ParsedAttribute, String )
parseAttributeList input =
    case parseAttributes input of
        Ok ( attributes, remainingInput ) ->
            Ok ( Types.ParsedAttributeList attributes, remainingInput )

        Err msg ->
            Err msg


typeNameRegex : Regex
typeNameRegex =
    Regex.regex "^[A-Z_][A-Z_0-9]*(?=\\()"


parseTypedAttribute : String -> Result String ( Types.ParsedAttribute, String )
parseTypedAttribute input =
    case find typeNameRegex input of
        [ { match } ] ->
            let
                inputPastParenthesis =
                    String.dropLeft (String.length match + 1) input
            in
            case parseAttribute inputPastParenthesis of
                Ok ( parsedAttribute, remainingInput ) ->
                    if String.startsWith ")" remainingInput then
                        Ok
                            ( Types.ParsedTypedAttribute
                                (Types.TypeName match)
                                parsedAttribute
                            , String.dropLeft 1 remainingInput
                            )
                    else
                        err "Could not parse typed attribute" input

                err ->
                    err

        _ ->
            err "Could not parse typed attribute " input


parseAttribute : String -> Result String ( Types.ParsedAttribute, String )
parseAttribute input =
    case String.left 1 input of
        "*" ->
            Ok ( Types.ParsedDefaultAttribute, String.dropLeft 1 input )

        "$" ->
            Ok ( Types.ParsedNullAttribute, String.dropLeft 1 input )

        "#" ->
            parseReference input

        "+" ->
            parseNumber input

        "-" ->
            parseNumber input

        "1" ->
            parseNumber input

        "2" ->
            parseNumber input

        "3" ->
            parseNumber input

        "4" ->
            parseNumber input

        "5" ->
            parseNumber input

        "6" ->
            parseNumber input

        "7" ->
            parseNumber input

        "8" ->
            parseNumber input

        "9" ->
            parseNumber input

        "0" ->
            parseNumber input

        "'" ->
            parseString input

        "." ->
            parseEnum input

        "\"" ->
            parseBinary input

        "(" ->
            parseAttributeList input

        _ ->
            parseTypedAttribute input


parseEntity : String -> Result String ( Types.ParsedEntity, String )
parseEntity input =
    case find typeNameRegex input of
        [ { match } ] ->
            case parseAttributes (dropMatch match input) of
                Ok ( parsedAttributes, remainingInput ) ->
                    Ok
                        ( { typeName = Types.TypeName match
                          , parsedAttributes = parsedAttributes
                          }
                        , remainingInput
                        )

                Err msg ->
                    Err msg

        _ ->
            err "Could not parse entity" input


entityInstanceRegex : Regex
entityInstanceRegex =
    Regex.regex "#(\\d+)=([A-Z_][A-Z_0-9]*)(?=\\()"


parseEntityInstance : String -> Result String ( ( Int, Types.ParsedEntity ), String )
parseEntityInstance input =
    case find entityInstanceRegex input of
        [ { match, submatches } ] ->
            case submatches of
                [ Just idString, Just typeNameString ] ->
                    case String.toInt idString of
                        Ok id ->
                            case parseAttributes (dropMatch match input) of
                                Ok ( parsedAttributes, remainingInput ) ->
                                    if String.startsWith ";" remainingInput then
                                        let
                                            typeName =
                                                Types.TypeName typeNameString

                                            parsedEntity =
                                                { typeName = typeName
                                                , parsedAttributes =
                                                    parsedAttributes
                                                }
                                        in
                                        Ok
                                            ( ( id, parsedEntity )
                                            , String.dropLeft 1 remainingInput
                                            )
                                    else
                                        err "Could not parse entity instance" input

                                Err msg ->
                                    Err msg

                        Err _ ->
                            err "Could not parse entity instance" input

                _ ->
                    err "Could not parse entity instance" input

        _ ->
            err "Could not parse entity instance" input


parseEntityInstances : String -> List ( Int, Types.ParsedEntity ) -> Result String (List ( Int, Types.ParsedEntity ))
parseEntityInstances input accumulated =
    if String.startsWith "#" input then
        case parseEntityInstance input of
            Ok ( entityInstance, remainingInput ) ->
                parseEntityInstances
                    remainingInput
                    (entityInstance :: accumulated)

            Err msg ->
                Err msg
    else if input == "ENDSEC;END-ISO-10303-21;" then
        Ok (List.reverse accumulated)
    else
        err "Could not parse entity instances" input


parseHeader : String -> Result String ( Header, String )
parseHeader input =
    let
        parserWithOffset =
            Parser.succeed (,)
                |= Parse.header
                |= Parser.LowLevel.getOffset
    in
    case Parser.run parserWithOffset input of
        Ok ( header, offset ) ->
            Ok ( header, String.dropLeft offset input )

        Err _ ->
            err "Could not parse header" input


file : String -> Result Error File
file string =
    let
        input =
            Parse.prepareString string
    in
    if String.startsWith "ISO-10303-21;HEADER;" input then
        case parseHeader (String.dropLeft 20 input) of
            Ok ( header, remainingInput ) ->
                if String.startsWith "ENDSEC;DATA;" remainingInput then
                    case parseEntityInstances (String.dropLeft 12 remainingInput) [] of
                        Ok parsedEntityInstances ->
                            case EntityResolution.resolve parsedEntityInstances of
                                Ok entities ->
                                    Ok <|
                                        Types.File
                                            { header = header
                                            , entities = entities
                                            , contents = string
                                            }

                                Err (EntityResolution.NonexistentEntity id) ->
                                    Err (NonexistentEntity id)

                                Err (EntityResolution.CircularReference chain) ->
                                    Err (CircularReference chain)

                        Err msg ->
                            Err (SyntaxError msg)
                else
                    err "Could not parse file" input
                        |> Result.mapError SyntaxError

            Err msg ->
                Err (SyntaxError msg)
    else
        err "Could not parse file" input
            |> Result.mapError SyntaxError
