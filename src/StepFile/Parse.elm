module StepFile.Parse exposing (Error(..), file, header)

{-| Functionality for parsing STEP files.

@docs Error, file, header, prepareString

-}

import Char
import Parser exposing ((|.), (|=), Parser)
import StepFile exposing (Entity, File, Header)
import StepFile.EntityResolution as EntityResolution
import StepFile.Types as Types
import String


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


comment : Parser ()
comment =
    Parser.multiComment "/*" "*/" Parser.NotNestable


whitespace : Parser ()
whitespace =
    Parser.oneOf [ Parser.spaces, comment ]


list : Parser a -> Parser (List a)
list parseItem =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = whitespace
        , item = parseItem
        , trailing = Parser.Forbidden
        }


isUpper : Char -> Bool
isUpper char =
    Char.isUpper char || char == '_'


isUpperOrDigit : Char -> Bool
isUpperOrDigit char =
    isUpper char || Char.isDigit char


keyword : Parser String
keyword =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf isUpper
            |. Parser.chompWhile isUpperOrDigit


typeName : Parser Types.TypeName
typeName =
    Parser.succeed Types.TypeName |= keyword


string : Parser String
string =
    Parser.succeed identity
        |. Parser.token "'"
        |= Parser.getChompedString (Parser.chompWhile ((/=) '\''))
        |. Parser.token "'"


binary : Parser String
binary =
    Parser.succeed identity
        |. Parser.token "\""
        |= Parser.getChompedString (Parser.chompWhile ((/=) '"'))
        |. Parser.token "\""


enum : Parser Types.EnumName
enum =
    Parser.succeed Types.EnumName
        |. Parser.token "."
        |= keyword
        |. Parser.token "."


id : Parser Int
id =
    Parser.succeed identity
        |. Parser.token "#"
        |= Parser.int


optionalSign : Parser Int
optionalSign =
    Parser.oneOf
        [ Parser.succeed -1 |. Parser.token "-"
        , Parser.succeed 1 |. Parser.token "+"
        , Parser.succeed 1
        ]


signedInt : Int -> Int -> Types.ParsedAttribute
signedInt sign value =
    Types.ParsedIntAttribute (sign * value)


signedFloat : Int -> Float -> Types.ParsedAttribute
signedFloat sign value =
    Types.ParsedFloatAttribute (toFloat sign * value)


numericAttribute : Parser Types.ParsedAttribute
numericAttribute =
    optionalSign
        |> Parser.andThen
            (\sign ->
                Parser.number
                    { int = Just (signedInt sign)
                    , float = Just (signedFloat sign)
                    , hex = Nothing
                    , octal = Nothing
                    , binary = Nothing
                    }
            )


typedAttribute : Parser Types.ParsedAttribute
typedAttribute =
    Parser.succeed Types.ParsedTypedAttribute
        |= typeName
        |. whitespace
        |. Parser.token "("
        |. whitespace
        |= lazyAttribute
        |. whitespace
        |. Parser.token ")"


attribute : Parser Types.ParsedAttribute
attribute =
    Parser.oneOf
        [ Parser.succeed Types.ParsedDefaultAttribute |. Parser.token "*"
        , Parser.succeed Types.ParsedNullAttribute |. Parser.token "$"
        , Parser.succeed (Types.ParsedBoolAttribute True) |. Parser.token ".T."
        , Parser.succeed (Types.ParsedBoolAttribute False) |. Parser.token ".F."
        , Parser.succeed Types.ParsedEnumAttribute |= enum
        , numericAttribute
        , Parser.succeed Types.ParsedStringAttribute |= string
        , Parser.succeed Types.ParsedBinaryAttribute |= binary
        , Parser.succeed Types.ParsedReference |= id
        , Parser.succeed Types.ParsedAttributeList |= list lazyAttribute
        , typedAttribute
        ]


lazyAttribute : Parser Types.ParsedAttribute
lazyAttribute =
    Parser.lazy (\() -> attribute)


entity : Parser Types.ParsedEntity
entity =
    Parser.succeed Types.ParsedEntity
        |= typeName
        |. whitespace
        |= list attribute


entityInstance : Parser ( Int, Types.ParsedEntity )
entityInstance =
    Parser.succeed Tuple.pair
        |= id
        |. whitespace
        |. Parser.token "="
        |. whitespace
        |= entity


entities : Parser (List ( Int, Types.ParsedEntity ))
entities =
    Parser.sequence
        { start = "DATA;"
        , item = entityInstance
        , separator = ";"
        , trailing = Parser.Mandatory
        , spaces = whitespace
        , end = "ENDSEC;"
        }


{-| -}
header : Parser Header
header =
    let
        start name =
            Parser.token (name ++ "(")

        end =
            Parser.token ");"

        comma =
            Parser.token ","

        stringList =
            list string
    in
    Parser.succeed
        (\fileDescription fileName timeStamp author organization preprocessorVersion originatingSystem authorization schemaIdentifiers ->
            Types.Header
                { fileDescription = fileDescription
                , fileName = fileName
                , timeStamp = timeStamp
                , author = author
                , organization = organization
                , preprocessorVersion = preprocessorVersion
                , originatingSystem = originatingSystem
                , authorization = authorization
                , schemaIdentifiers = schemaIdentifiers
                }
        )
        |. Parser.token "HEADER;"
        |. start "FILE_DESCRIPTION"
        |= stringList
        |. comma
        |. Parser.token "'2;1'"
        |. end
        |. start "FILE_NAME"
        |= string
        |. comma
        |= string
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
        |. Parser.token "ENDSEC;"


fileParser : Parser ( Header, List ( Int, Types.ParsedEntity ) )
fileParser =
    Parser.succeed Tuple.pair
        |. Parser.token "ISO-10303-21;"
        |= header
        |= entities
        |. Parser.token "END-ISO-10303-21;"
        |. Parser.end


toSyntaxError : List Parser.DeadEnd -> Error
toSyntaxError deadEnds =
    SyntaxError (Parser.deadEndsToString deadEnds)


extractResolutionError : EntityResolution.Error -> Error
extractResolutionError resolutionError =
    case resolutionError of
        EntityResolution.NonexistentEntity id_ ->
            NonexistentEntity id_

        EntityResolution.CircularReference chain ->
            CircularReference chain


{-| Attempt to parse a string of text loaded from a STEP file.
-}
file : String -> Result Error File
file contents =
    Parser.run fileParser contents
        |> Result.mapError toSyntaxError
        |> Result.andThen
            (\( header_, parsedEntityInstances ) ->
                EntityResolution.resolve parsedEntityInstances
                    |> Result.mapError extractResolutionError
                    |> Result.map
                        (\entities_ ->
                            Types.File
                                { header = header_
                                , entities = entities_
                                , contents = contents
                                }
                        )
            )
