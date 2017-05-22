module OpenSolid.Step.Parse exposing (file)

import OpenSolid.Step exposing (Header, Entity)
import OpenSolid.Step.Types as Types
import OpenSolid.Step.Format as Format
import Parser exposing (Parser, (|.), (|=))
import Parser.LanguageKit
import Char
import String
import String.Extra as String
import Bitwise


type ParsedAttribute
    = ParsedDefaultAttribute
    | ParsedNullAttribute
    | ParsedIntAttribute Int
    | ParsedFloatAttribute Float
    | ParsedStringAttribute String
    | ParsedBinaryAttribute String
    | ParsedEnumAttribute Types.EnumName
    | ParsedReference Int
    | ParsedTypedAttribute Types.TypeName ParsedAttribute
    | ParsedAttributeList (List ParsedAttribute)


type alias ParsedEntity =
    { typeName : String
    , parsedAttributes : List ParsedAttribute
    }


isValidCharacter : Char -> Bool
isValidCharacter character =
    let
        code =
            Char.toCode character
    in
        (code >= 0x20 && code <= 0x7E) || (code >= 0x80)


file : String -> Result String ( Header, List ParsedEntity )
file string =
    let
        contents =
            String.filter isValidCharacter string

        parser : Parser ( Header, List ParsedEntity )
        parser =
            Parser.succeed (,)
                |. Parser.keyword "ISO-1303-21;"
                |. whitespace
                |= header
                |. whitespace
                |= entities
                |. whitespace
                |. Parser.keyword "END-ISO-10303-21;"
                |. whitespace
                |. Parser.end
    in
        Err "Not implemented"


header : Parser Header
header =
    Parser.fail "Not implemented"



--    Parser.succeed
--        (\fileDescriptionEntity fileNameEntity fileSchemaEntity ->
--            ()
--        )
--        |. Parser.keyword "HEADER;"
--        |. whitespace
--        |. Parser.keyword "ENDSEC;"


entities : Parser (List ParsedEntity)
entities =
    Parser.fail "Not implemented"


whitespace : Parser ()
whitespace =
    let
        spaces =
            Parser.ignore Parser.zeroOrMore (\character -> character == ' ')

        comment =
            Parser.symbol "/*" |. Parser.ignoreUntil "*/"
    in
        Parser.repeat Parser.zeroOrMore (Parser.oneOf [ spaces, comment ])
            |> Parser.andThen (always (Parser.succeed ()))


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


enumName : Parser Types.EnumName
enumName =
    keyword |> Parser.map Types.EnumName


isBasic : Char -> Bool
isBasic character =
    let
        code =
            Char.toCode character
    in
        (code >= 0x20 && code <= 0x7E)
            && (character /= '\'')
            && (character /= '\\')


isHexCharacter : Char -> Bool
isHexCharacter character =
    (character >= '0' && character <= '9')
        || (character >= 'A' && character <= 'F')


hexDigit : Parser Int
hexDigit =
    Parser.oneOf
        [ Parser.symbol "0" |> Parser.map (\() -> 0)
        , Parser.symbol "1" |> Parser.map (\() -> 1)
        , Parser.symbol "2" |> Parser.map (\() -> 2)
        , Parser.symbol "3" |> Parser.map (\() -> 3)
        , Parser.symbol "4" |> Parser.map (\() -> 4)
        , Parser.symbol "5" |> Parser.map (\() -> 5)
        , Parser.symbol "6" |> Parser.map (\() -> 6)
        , Parser.symbol "7" |> Parser.map (\() -> 7)
        , Parser.symbol "8" |> Parser.map (\() -> 8)
        , Parser.symbol "9" |> Parser.map (\() -> 9)
        , Parser.symbol "A" |> Parser.map (\() -> 10)
        , Parser.symbol "B" |> Parser.map (\() -> 11)
        , Parser.symbol "C" |> Parser.map (\() -> 12)
        , Parser.symbol "D" |> Parser.map (\() -> 13)
        , Parser.symbol "E" |> Parser.map (\() -> 14)
        , Parser.symbol "F" |> Parser.map (\() -> 15)
        ]


x0 : Int -> Int -> String
x0 high low =
    String.fromCodePoints [ Bitwise.shiftLeftBy 4 high + low ]


x2 : List ( Int, Int, Int, Int ) -> String
x2 hexDigits =
    let
        codePoint ( a, b, c, d ) =
            d
                + (Bitwise.shiftLeftBy 4 c)
                + (Bitwise.shiftLeftBy 8 b)
                + (Bitwise.shiftLeftBy 12 a)
    in
        String.fromCodePoints (List.map codePoint hexDigits)


x4 : List ( Int, Int, Int, Int, Int, Int ) -> String
x4 hexDigits =
    let
        codePoint ( a, b, c, d, e, f ) =
            f
                + (Bitwise.shiftLeftBy 4 e)
                + (Bitwise.shiftLeftBy 8 d)
                + (Bitwise.shiftLeftBy 12 c)
                + (Bitwise.shiftLeftBy 16 b)
                + (Bitwise.shiftLeftBy 20 a)
    in
        String.fromCodePoints (List.map codePoint hexDigits)


string : Parser String
string =
    Parser.succeed String.concat
        |. Parser.symbol "'"
        |= Parser.repeat Parser.zeroOrMore
            (Parser.oneOf
                [ Parser.symbol "''" |> Parser.map (\() -> "'")
                , Parser.symbol "\\\\" |> Parser.map (\() -> "\\")
                , Parser.succeed x0
                    |. Parser.symbol "\\X\\"
                    |= hexDigit
                    |= hexDigit
                , Parser.succeed x2
                    |. Parser.symbol "\\X2\\"
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,,,)
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                        )
                    |. Parser.symbol "\\X0\\"
                , Parser.succeed x4
                    |. Parser.symbol "\\X4\\"
                    |= Parser.repeat Parser.oneOrMore
                        (Parser.succeed (,,,,,)
                            |. Parser.symbol "0"
                            |. Parser.symbol "0"
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                            |= hexDigit
                        )
                    |. Parser.symbol "\\X0\\"
                , Parser.keep Parser.oneOrMore isBasic
                ]
            )
        |. Parser.symbol "'"


attribute : Parser ParsedAttribute
attribute =
    let
        defaultAttribute =
            Parser.symbol "*"
                |> Parser.map (\() -> ParsedDefaultAttribute)

        nullAttribute =
            Parser.symbol "$"
                |> Parser.map (\() -> ParsedNullAttribute)

        numericAttribute =
            Parser.oneOf
                [ Parser.symbol "+"
                    |> Parser.andThen (\() -> Parser.float)
                , Parser.symbol "-"
                    |> Parser.andThen (\() -> Parser.float)
                    |> Parser.map negate
                ]
                |> Parser.sourceMap
                    (\rawString parsedFloat ->
                        if String.contains "." rawString then
                            ParsedFloatAttribute parsedFloat
                        else
                            ParsedIntAttribute (round parsedFloat)
                    )

        stringAttribute =
            string |> Parser.map ParsedStringAttribute

        binaryAttribute =
            Parser.succeed identity
                |. Parser.symbol "\""
                |= Parser.keep Parser.oneOrMore isHexCharacter
                |. Parser.symbol "\""
                |> Parser.map ParsedBinaryAttribute

        enumAttribute =
            Parser.succeed identity
                |. Parser.symbol "."
                |= enumName
                |. Parser.symbol "."
                |> Parser.map ParsedEnumAttribute

        unevaluatedReference =
            Parser.succeed identity
                |. Parser.symbol "#"
                |= Parser.int
                |> Parser.map ParsedReference

        typedAttribute =
            Parser.succeed ParsedTypedAttribute
                |= typeName
                |. Parser.symbol "("
                |= Parser.lazy (\() -> attribute)
                |. Parser.symbol ")"

        attributeList =
            Parser.LanguageKit.tuple whitespace
                (Parser.lazy (\() -> attribute))
                |> Parser.map ParsedAttributeList
    in
        Parser.oneOf
            [ defaultAttribute
            , nullAttribute
            , numericAttribute
            , stringAttribute
            , binaryAttribute
            , enumAttribute
            , unevaluatedReference
            , typedAttribute
            , attributeList
            ]
