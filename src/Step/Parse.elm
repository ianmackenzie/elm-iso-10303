module Step.Parse exposing (entity, header, hexStringToBytes, whitespace)

import Bytes exposing (Bytes)
import Bytes.Encode
import Char
import Parser exposing ((|.), (|=), Parser)
import Step.EntityResolution as EntityResolution
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.Internal exposing (ParsedAttribute(..), ParsedEntity(..))
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types exposing (Header)
import String


comment : Parser ()
comment =
    -- TODO remove final 'token' call once multiComment is fixed
    Parser.multiComment "/*" "*/" Parser.NotNestable
        |. Parser.token "*/"


whitespace : Parser ()
whitespace =
    Parser.spaces
        |. Parser.loop ()
            (\() ->
                Parser.oneOf
                    [ Parser.succeed (Parser.Loop ())
                        |. comment
                        |. Parser.spaces
                    , Parser.succeed (Parser.Done ())
                    ]
            )


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
    Parser.getChompedString
        (Parser.chompIf isUpper |. Parser.chompWhile isUpperOrDigit)


typeName : Parser TypeName
typeName =
    Parser.succeed TypeName.fromString |= keyword


string : Parser String
string =
    Parser.getChompedString
        (Parser.token "'"
            |. Parser.loop ()
                (\() ->
                    Parser.succeed identity
                        |. Parser.chompUntil "'"
                        -- TODO remove if chompUntil is changed
                        |. Parser.token "'"
                        |= Parser.oneOf
                            [ Parser.succeed (Parser.Loop ())
                                |. Parser.token "'"
                            , Parser.succeed (Parser.Done ())
                            ]
                )
        )
        -- Strip off start/end single quotation marks
        |> Parser.map (String.slice 1 -1)


hexValue : Char -> Int
hexValue char =
    let
        charCode =
            Char.toCode char
    in
    if charCode >= 48 && charCode <= 57 then
        charCode - 48

    else if charCode >= 65 && charCode <= 70 then
        charCode - 55

    else
        -- Shouldn't happen
        0


byteValues : List Int -> List Int -> List Int
byteValues accumulated hexValues =
    case hexValues of
        first :: second :: rest ->
            byteValues ((first + 16 * second) :: accumulated) rest

        [ single ] ->
            single :: accumulated

        [] ->
            accumulated


hexStringToBytes : String -> Bytes
hexStringToBytes hexString =
    hexString
        -- ignore leading 'number of zero padding bits' value since elm/bytes
        -- only supports byte-aligned binary data
        |> String.dropLeft 1
        -- Normalize string to upper case
        |> String.toUpper
        -- Convert to list of character hex values
        |> String.toList
        |> List.map hexValue
        -- Combine pairs of (assumed hex) characters into byte values,
        -- starting from least significant
        |> List.reverse
        |> byteValues []
        -- Encode list of bytes as Bytes value
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


binary : Parser Bytes
binary =
    Parser.succeed hexStringToBytes
        |. Parser.token "\""
        |= Parser.getChompedString (Parser.chompWhile ((/=) '"'))
        |. Parser.token "\""


enum : Parser EnumValue
enum =
    Parser.succeed EnumValue.fromString
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


signedInt : Int -> Int -> ParsedAttribute
signedInt sign value =
    ParsedIntAttribute (sign * value)


signedFloat : Int -> Float -> ParsedAttribute
signedFloat sign value =
    ParsedFloatAttribute (toFloat sign * value)


numericAttribute : Parser ParsedAttribute
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


typedAttribute : Parser ParsedAttribute
typedAttribute =
    Parser.succeed ParsedTypedAttribute
        |= typeName
        |. whitespace
        |. Parser.token "("
        |. whitespace
        |= lazyAttribute
        |. whitespace
        |. Parser.token ")"


attribute : Parser ParsedAttribute
attribute =
    Parser.oneOf
        [ Parser.succeed ParsedDerivedAttribute |. Parser.token "*"
        , Parser.succeed ParsedNullAttribute |. Parser.token "$"
        , Parser.succeed (ParsedBoolAttribute True) |. Parser.token ".T."
        , Parser.succeed (ParsedBoolAttribute False) |. Parser.token ".F."
        , Parser.succeed ParsedEnumAttribute |= enum
        , numericAttribute
        , Parser.succeed ParsedStringAttribute |= string
        , Parser.succeed ParsedBinaryAttribute |= binary
        , Parser.succeed ParsedReference |= id
        , Parser.succeed ParsedAttributeList |= list lazyAttribute
        , typedAttribute
        ]


lazyAttribute : Parser ParsedAttribute
lazyAttribute =
    Parser.lazy (\() -> attribute)


entity : Parser ( Int, ParsedEntity )
entity =
    Parser.succeed
        (\id_ typeName_ attributes_ ->
            ( id_, ParsedSimpleEntity typeName_ attributes_ )
        )
        |= id
        |. Parser.token "="
        |= typeName
        |= list attribute


header : Parser Header
header =
    let
        start name =
            Parser.token name |. whitespace |. Parser.token "("

        end =
            Parser.token ");"

        comma =
            Parser.token ","

        stringList =
            list string
    in
    Parser.succeed Header
        |. Parser.token "HEADER;"
        |. whitespace
        |. start "FILE_DESCRIPTION"
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. end
        |. whitespace
        |. start "FILE_NAME"
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. end
        |. whitespace
        |. start "FILE_SCHEMA"
        |. whitespace
        |= stringList
        |. whitespace
        |. end
        |. whitespace
        |. Parser.token "ENDSEC;"
