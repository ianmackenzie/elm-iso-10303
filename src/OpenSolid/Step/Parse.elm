module OpenSolid.Step.Parse exposing (file)

import OpenSolid.Step exposing (Header, Entity)
import OpenSolid.Step.Types as Types
import Parser exposing (Parser, (|.), (|=))
import Parser.LanguageKit
import Char


isValidCharacter : Char -> Bool
isValidCharacter character =
    let
        code =
            Char.toCode character
    in
        (code >= 0x20 && code <= 0x7E) || (code >= 0x80)


file : String -> Result String ( Header, List Entity )
file string =
    let
        contents =
            String.filter isValidCharacter string
    in
        Err "Not implemented"


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\character -> character == ' ')


comment : Parser ()
comment =
    Parser.symbol "/*" |. Parser.ignoreUntil "*/"


whitespace : Parser ()
whitespace =
    Parser.repeat Parser.zeroOrMore (Parser.oneOf [ spaces, comment ])
        |> Parser.andThen (always (Parser.succeed ()))
