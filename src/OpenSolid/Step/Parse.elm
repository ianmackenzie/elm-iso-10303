module OpenSolid.Step.Parse exposing (Error(..), file)

{-| Functionality for parsing STEP files to produce a `Header` and a list of
`Entity` values.

@docs Error, file

-}

import Bitwise
import Char
import Date exposing (Date)
import Dict exposing (Dict)
import OpenSolid.Step exposing (Entity, Header)
import OpenSolid.Step.EntityResolution as EntityResolution
import OpenSolid.Step.Types as Types
import Parser exposing ((|.), (|=), Parser)
import Regex
import String
import String.Extra as String


{-| Types of errors that can be encountered when parsing a file:

  - A `ParseError` means an error actually parsing STEP text; this means that
    either the STEP file is improperly formatted or (more likely!) it uses
    an aspect of STEP syntax that is not yet supported by this package. The
    three parameters are the row and column number where the error occurred,
    and an error string that can be used for debugging (not suitable to be
    shown to end users).
  - A `ResolveError` means that the file was parsed OK, but an error occurred
    when a reference such as `#23` was found in one entity but no entity with
    that ID existed in the file. The integer parameter is the ID of the
    nonexistent entity.

-}
type Error
    = ParseError Int Int String
    | ResolveError Int


comma : Parser ()
comma =
    Parser.symbol ","


list : Parser a -> Parser (List a)
list item =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Parser.oneOf
            [ Parser.symbol ")" |> Parser.map (\() -> [])
            , Parser.succeed (\first rest -> first :: rest)
                |= item
                |= Parser.repeat Parser.zeroOrMore
                    (Parser.succeed identity
                        |. comma
                        |= item
                    )
                |. Parser.symbol ")"
            ]


keyword : Parser String
keyword =
    let
        validFirstCharacter character =
            Char.isUpper character || character == '_'

        validOtherCharacter character =
            validFirstCharacter character || Char.isDigit character
    in
    Parser.source
        (Parser.ignore (Parser.Exactly 1) validFirstCharacter
            |. Parser.ignore Parser.zeroOrMore validOtherCharacter
        )


typeName : Parser Types.TypeName
typeName =
    keyword |> Parser.map Types.TypeName


string : Parser String
string =
    Parser.repeat Parser.oneOrMore
        (Parser.source (Parser.symbol "'" |. Parser.ignoreUntil "'"))
        |> Parser.map (String.concat >> String.slice 1 -1)


binary : Parser String
binary =
    Parser.source (Parser.symbol "\"" |. Parser.ignoreUntil "\"")
        |> Parser.map (String.slice 1 -1)


enum : Parser Types.EnumName
enum =
    Parser.source (Parser.symbol "." |. Parser.ignoreUntil ".")
        |> Parser.map (String.slice 1 -1 >> Types.EnumName)


id : Parser Int
id =
    Parser.succeed identity
        |. Parser.symbol "#"
        |= Parser.int


attribute : Parser Types.ParsedAttribute
attribute =
    let
        defaultAttribute =
            default |> Parser.map (\() -> Types.ParsedDefaultAttribute)
            Parser.symbol "*"
                |> Parser.map (\() -> Types.ParsedDefaultAttribute)

        nullAttribute =
            Parser.symbol "$"
                |> Parser.map (\() -> Types.ParsedNullAttribute)
        trueAttribute =
            Parser.keyword ".T."
                |> Parser.map (\() -> Types.ParsedBoolAttribute True)

        falseAttribute =
            Parser.keyword ".F."
                |> Parser.map (\() -> Types.ParsedBoolAttribute False)

        optionalSign =
            Parser.oneOf
                [ Parser.symbol "+", Parser.symbol "-", Parser.succeed () ]

        digits =
            Parser.ignore Parser.oneOrMore Char.isDigit

        optionalDigits =
            Parser.ignore Parser.zeroOrMore Char.isDigit

        numericAttribute =
            Parser.sourceMap (,)
                (Parser.succeed identity
                    |. optionalSign
                    |. digits
                    |= Parser.oneOf
                        [ Parser.succeed True
                            |. Parser.symbol "."
                            |. optionalDigits
                            |. Parser.oneOf
                                [ Parser.symbol "E" |. optionalSign |. digits
                                , Parser.succeed ()
                                ]
                        , Parser.succeed False
                        ]
                )
                |> Parser.andThen
                    (\( string, isFloat ) ->
                        if isFloat then
                            case String.toFloat string of
                                Ok value ->
                                    Parser.succeed
                                        (Types.ParsedFloatAttribute value)

                                Err message ->
                                    Parser.fail message
                        else
                            case String.toInt string of
                                Ok value ->
                                    Parser.succeed
                                        (Types.ParsedIntAttribute value)

                                Err message ->
                                    Parser.fail message
                    )

        stringAttribute =
            string |> Parser.map Types.ParsedStringAttribute

        binaryAttribute =
            binary |> Parser.map Types.ParsedBinaryAttribute

        enumAttribute =
            enum |> Parser.map Types.ParsedEnumAttribute

        unevaluatedReference =
            id |> Parser.map Types.ParsedReference

        typedAttribute =
            Parser.succeed Types.ParsedTypedAttribute
                |= typeName
                |. Parser.symbol "("
                |= Parser.lazy (\() -> attribute)
                |. Parser.symbol ")"

        attributeList =
            list (Parser.lazy (\() -> attribute))
                |> Parser.map Types.ParsedAttributeList
    in
    Parser.oneOf
        [ defaultAttribute
        , nullAttribute
        , trueAttribute
        , falseAttribute
        , enumAttribute
        , numericAttribute
        , stringAttribute
        , binaryAttribute
        , unevaluatedReference
        , typedAttribute
        , attributeList
        ]


entity : Parser Types.ParsedEntity
entity =
    Parser.succeed Types.ParsedEntity
        |= typeName
        |= list attribute


entityInstance : Parser ( Int, Types.ParsedEntity )
entityInstance =
    Parser.succeed (,)
        |= id
        |. Parser.symbol "="
        |= entity
        |. Parser.symbol ";"


date : Parser Date
date =
    let
        toDate string =
            Date.fromString string |> Result.withDefault (Date.fromTime 0)
    in
    Parser.succeed toDate
        |. Parser.symbol "'"
        |= Parser.source
            (Parser.succeed ()
                |. Parser.ignore (Parser.Exactly 4) Char.isDigit
                |. Parser.symbol "-"
                |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                |. Parser.symbol "-"
                |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                |. Parser.symbol "T"
                |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                |. Parser.symbol ":"
                |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                |. Parser.symbol ":"
                |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                |. Parser.oneOf
                    [ Parser.succeed ()
                        |. Parser.oneOf [ Parser.symbol "+", Parser.symbol "-" ]
                        |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                        |. Parser.symbol ":"
                        |. Parser.ignore (Parser.Exactly 2) Char.isDigit
                    , Parser.succeed ()
                    ]
            )
        |. Parser.symbol "'"


header : Parser Header
header =
    let
        start typeName =
            Parser.symbol (typeName ++ "(")

        end =
            Parser.symbol ");"

        stringList =
            list string
    in
    Parser.succeed Header
        |. start "FILE_DESCRIPTION"
        |= stringList
        |. comma
        |. Parser.keyword "'2;1'"
        |. end
        |. start "FILE_NAME"
        |= string
        |. comma
        |= date
        |. comma
        |= stringList
        |. comma
        |= stringList
        |. comma
        |= string
        |. comma
        |= string
        |. comma
        |= string
        |. end
        |. start "FILE_SCHEMA"
        |= stringList
        |. end


fileParser : Parser ( Header, List ( Int, Types.ParsedEntity ) )
fileParser =
    Parser.succeed (,)
        |. Parser.keyword "ISO-10303-21;"
        |. Parser.keyword "HEADER;"
        |= header
        |. Parser.keyword "ENDSEC;"
        |. Parser.keyword "DATA;"
        |= Parser.repeat Parser.zeroOrMore entityInstance
        |. Parser.keyword "ENDSEC;"
        |. Parser.keyword "END-ISO-10303-21;"
        |. Parser.end


{-| Pre-process the file contents to make them easier to parse
-}
prepareString : String -> String
prepareString =
    let
        joinLines =
            String.lines >> String.join ""

        removeMatches regex =
            Regex.replace Regex.All regex (always "")

        commentRegex =
            Regex.regex "/\\*.*?\\*/"

        stripComments =
            removeMatches commentRegex

        whitespaceRegex =
            Regex.regex "[ \\n\\r]+"

        chunkRegex =
            Regex.regex "(?:[^']+)|(?:'[^']*')"

        chunkReplacement { match } =
            if String.startsWith "'" match then
                -- Don't do anything to string chunks
                match
            else
                -- Strip whitespace out of everything else
                removeMatches whitespaceRegex match

        stripWhitespaceOutsideStrings =
            Regex.replace Regex.All chunkRegex chunkReplacement
    in
    joinLines >> stripComments >> stripWhitespaceOutsideStrings


toParseError : Parser.Error -> Error
toParseError { row, col, problem } =
    ParseError row col (toString problem)


toResolveError : EntityResolution.Error -> Error
toResolveError (EntityResolution.NonexistentEntity id) =
    ResolveError id


{-| Attempt to parse a string of text loaded from a STEP file. On success,
returns a record containing information from the file header and a `Dict`
containing `Entity` values indexed by their ID.
-}
file : String -> Result Error ( Header, Dict Int Entity )
file string =
    Parser.run fileParser (prepareString string)
        |> Result.mapError toParseError
        |> Result.andThen
            (\( header, parsedEntityInstances ) ->
                EntityResolution.resolve parsedEntityInstances
                    |> Result.mapError toResolveError
                    |> Result.map (\entities -> ( header, entities ))
            )
