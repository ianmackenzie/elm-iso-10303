module OpenSolid.Step.FastParse exposing (..)

{-| Functionality for parsing STEP files.

@docs Error, file

-}

import OpenSolid.Step exposing (Entity, File, Header)
import OpenSolid.Step.Types as Types
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


stringRegex : Regex
stringRegex =
    Regex.regex "^'[^']*'"


parseString : String -> Result String ( Types.ParsedAttribute, String )
parseString input =
    case find stringRegex input of
        [ { match } ] ->
            Ok
                ( Types.ParsedStringAttribute (String.slice 1 -1 match)
                , String.dropLeft (String.length match) input
                )

        _ ->
            Err "Could not parse string"


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
            Err "Could not parse binary"


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
                            Err "Could not parse integer"

                _ ->
                    case String.toFloat match of
                        Ok value ->
                            Ok
                                ( Types.ParsedFloatAttribute value
                                , dropMatch match input
                                )

                        Err _ ->
                            Err "Could not parse float"

        _ ->
            Err "Could not parse number"


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
                Err "Could not parse enum"


referenceRegex : Regex
referenceRegex =
    Regex.regex "#\\d+"


parseReference : String -> Result String ( Types.ParsedAttribute, String )
parseReference input =
    case find referenceRegex input of
        [ { match } ] ->
            case String.toInt (String.dropLeft 1 match) of
                Ok value ->
                    Ok ( Types.ParsedReference value, dropMatch match input )

                Err _ ->
                    Err "Could not parse reference"

        _ ->
            Err "Could not parse reference"


parseAttributeList : String -> List Types.ParsedAttribute -> Result String ( Types.ParsedAttribute, String )
parseAttributeList input accumulated =
    case parseAttribute input of
        Ok ( parsedAttribute, remainingInput ) ->
            let
                prepended =
                    parsedAttribute :: accumulated

                followingInput =
                    String.dropLeft 1 remainingInput
            in
            if String.startsWith "," remainingInput then
                parseAttributeList followingInput prepended
            else if String.startsWith ")" remainingInput then
                Ok
                    ( Types.ParsedAttributeList (List.reverse prepended)
                    , followingInput
                    )
            else
                Err "Could not parse attribute list"

        err ->
            err


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
                        Err "Could not parse typed attribute"

                err ->
                    err

        _ ->
            Err "Could not parse typed attribute"


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
            parseAttributeList (String.dropLeft 1 input) []

        _ ->
            parseTypedAttribute input


file : String -> Result Error File
file string =
    Debug.crash "TODO"
