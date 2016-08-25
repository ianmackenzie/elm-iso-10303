module OpenSolid.StepFile exposing (toCodePoints, encode)

import String
import Char
import Bitwise


hexCharacterAtIndex : Int -> Int -> String
hexCharacterAtIndex index value =
    let
        hexDigit =
            Bitwise.and 0x0F (Bitwise.shiftRight value (index * 4))
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
            [1..size] |> List.map (\i -> hexCharacterAtIndex (size - i) value)
    in
        String.concat characters


toCodePoints : String -> List Int
toCodePoints string =
    let
        combineSurrogates codeUnits =
            case codeUnits of
                [] ->
                    []

                first :: afterFirst ->
                    -- We have at least one code unit - might be a code point
                    -- itself, or the leading code unit of a surrogate pair
                    if first >= 0 && first <= 0xD7FF then
                        -- First code unit is in BMP (and is therefore a valid
                        -- UTF-32 code point), use it as is and continue with
                        -- remaining code units
                        first :: combineSurrogates afterFirst
                    else if first >= 0xD800 && first <= 0xDBFF then
                        -- First code unit is a leading surrogate
                        case afterFirst of
                            [] ->
                                -- Should never happen - leading surrogate with
                                -- no following code unit, discard it
                                []

                            second :: afterSecond ->
                                -- Good, there is a following code unit (which
                                -- should be a trailing surrogate)
                                if second >= 0xDC00 && second <= 0xDFFF then
                                    -- Second code unit is a valid trailing
                                    -- surrogate
                                    let
                                        -- Reconstruct UTF-32 code point from
                                        -- surrogate pair
                                        codePoint =
                                            0x00010000
                                                + ((first - 0xD800) * 1024)
                                                + (second - 0xDC00)
                                    in
                                        -- Continue with following code units
                                        codePoint
                                            :: combineSurrogates afterSecond
                                else
                                    -- Should never happen - second code unit is
                                    -- not a valid trailing surrogate, skip the
                                    -- leading surrogate and continue with
                                    -- remaining code units
                                    combineSurrogates afterFirst
                    else if first >= 0xE000 && first <= 0xFFFF then
                        -- First code unit is in BMP (and is therefore a valid
                        -- UTF-32 code point), use it as is and continue with
                        -- remaining code units
                        first :: combineSurrogates afterFirst
                    else
                        -- Should never happen - first code unit is invalid,
                        -- skip it and continue with remaining code units
                        combineSurrogates afterFirst

        allCodeUnits =
            List.map Char.toCode (String.toList string)
    in
        combineSurrogates allCodeUnits


encode : String -> String
encode string =
    let
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
    in
        String.concat (List.map codePointToString (toCodePoints string))
