module OpenSolid.Step.Encode
    exposing
        ( float
        , string
        , date
        )

import String
import String.Extra
import Char
import Bitwise
import Date exposing (Date, Month)


float : Float -> String
float value =
    if toFloat (round value) == value then
        -- STEP requires integer-valued floats to have a trailing '.'
        toString value ++ "."
    else
        toString value


hexCharacterAtIndex : Int -> Int -> String
hexCharacterAtIndex index value =
    let
        hexDigit =
            Bitwise.and 0x0F (Bitwise.shiftRightBy (index * 4) value)
    in
        if hexDigit >= 0 && hexDigit <= 9 then
            String.fromChar (Char.fromCode (Char.toCode '0' + hexDigit))
        else if hexDigit >= 0 && hexDigit <= 15 then
            String.fromChar (Char.fromCode (Char.toCode 'A' + (hexDigit - 10)))
        else
            ""


hexEncode : Int -> Int -> String
hexEncode size value =
    let
        characters =
            List.range 1 size
                |> List.map (\i -> hexCharacterAtIndex (size - i) value)
    in
        String.concat characters


codePointToString : Int -> String
codePointToString codePoint =
    if (codePoint >= 0 && codePoint <= 0x1F) then
        "\\X\\" ++ hexEncode 2 codePoint
    else if (codePoint >= 0x20 && codePoint <= 0x7E) then
        String.fromChar (Char.fromCode codePoint)
    else if (codePoint >= 0x7F && codePoint <= 0xFF) then
        "\\X\\" ++ hexEncode 2 codePoint
    else if (codePoint >= 0x0100 && codePoint <= 0xFFFF) then
        "\\X2\\" ++ hexEncode 4 codePoint ++ "\\X0\\"
    else if (codePoint >= 0x00010000 && codePoint <= 0x0010FFFF) then
        "\\X4\\" ++ hexEncode 8 codePoint ++ "\\X0\\"
    else
        ""


string : String -> String
string str =
    String.Extra.toCodePoints str
        |> List.map codePointToString
        |> String.concat


month : Month -> String
month month_ =
    case month_ of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


twoDigits : Int -> String
twoDigits value =
    if value < 10 then
        "0" ++ toString value
    else
        toString value


date : Date -> String
date date_ =
    String.join ""
        [ toString (Date.year date_)
        , "-"
        , month (Date.month date_)
        , "-"
        , twoDigits (Date.day date_)
        , "T"
        , twoDigits (Date.hour date_)
        , ":"
        , twoDigits (Date.minute date_)
        , ":"
        , twoDigits (Date.second date_)
        ]
