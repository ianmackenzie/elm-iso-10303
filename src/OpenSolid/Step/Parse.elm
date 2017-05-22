module OpenSolid.Step.Parse exposing (file)

import OpenSolid.Step exposing (Header, Entity)
import OpenSolid.Step.Types as Types
import OpenSolid.Step.Format as Format
import Parser exposing (Parser, (|.), (|=))
import Parser.LanguageKit
import Char


type ParsedAttribute
    = EvaluatedAttribute Types.Attribute
    | UnevaluatedReference Int


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


typeName : Parser String
typeName =
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


int : Parser Int
int =
    Parser.oneOf
        [ Parser.int
        , Parser.symbol "+"
            |> Parser.andThen (\() -> Parser.int)
        , Parser.symbol "-"
            |> Parser.andThen (\() -> Parser.int)
            |> Parser.map negate
        ]


float : Parser Float
float =
    Parser.oneOf
        [ Parser.float
        , Parser.symbol "+"
            |> Parser.andThen (\() -> Parser.float)
        , Parser.symbol "-"
            |> Parser.andThen (\() -> Parser.float)
            |> Parser.map negate
        ]



-- TODO add string, binary, enum, reference, typed attribute, attribute list


attribute : Parser ParsedAttribute
attribute =
    let
        defaultAttribute =
            Parser.symbol "*"
                |> Parser.map
                    (\() -> (EvaluatedAttribute Types.DefaultAttribute))

        nullAttribute =
            Parser.symbol "$"
                |> Parser.map
                    (\() -> (EvaluatedAttribute Types.NullAttribute))

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
                            EvaluatedAttribute
                                (Types.FloatAttribute parsedFloat)
                        else
                            EvaluatedAttribute
                                (Types.IntAttribute (round parsedFloat))
                    )
    in
        Parser.oneOf
            [ defaultAttribute
            , nullAttribute
            , numericAttribute
            ]
