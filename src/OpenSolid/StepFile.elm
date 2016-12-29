module OpenSolid.StepFile exposing (encode)

{-| Functions for generating STEP (ISO 10303-21) files.

@docs encode
-}

import String
import String.Extra
import Char
import Bitwise


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


{-| Encode a string for output to a STEP file.
-}
encode : String -> String
encode =
    String.Extra.toCodePoints
        >> List.map codePointToString
        >> String.concat
