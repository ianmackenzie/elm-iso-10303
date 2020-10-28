module Step.Format exposing (derivedValue, null, bool, int, float, string, id, enum, binary, list, typedAttribute)

{-| Low-level attribute formatting functionality. Usually you will want to use
the functions in the [`Step.Encode`](Step-Encode) module instead.

@docs derivedValue, null, bool, int, float, string, id, enum, binary, list, typedAttribute

-}

import Bitwise
import Char
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types as Types


string : String -> String
string value =
    let
        encoded =
            value
                |> String.toList
                |> List.map encodedCharacter
                |> String.concat
    in
    "'" ++ encoded ++ "'"


apostropheCodePoint : Int
apostropheCodePoint =
    Char.toCode '\''


backslashCodePoint : Int
backslashCodePoint =
    Char.toCode '\\'


encodedCharacter : Char -> String
encodedCharacter character =
    if character == '\'' then
        "''"

    else if character == '\\' then
        "\\"

    else
        let
            codePoint =
                Char.toCode character
        in
        if codePoint >= 0 && codePoint <= 0x1F then
            "\\X\\" ++ hexEncode 2 codePoint

        else if codePoint >= 0x20 && codePoint <= 0x7E then
            String.fromChar character

        else if codePoint >= 0x7F && codePoint <= 0xFF then
            "\\X\\" ++ hexEncode 2 codePoint

        else if codePoint >= 0x0100 && codePoint <= 0xFFFF then
            "\\X2\\" ++ hexEncode 4 codePoint ++ "\\X0\\"

        else if codePoint >= 0x00010000 && codePoint <= 0x0010FFFF then
            "\\X4\\" ++ hexEncode 8 codePoint ++ "\\X0\\"

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


binary : String -> String
binary value =
    "\"" ++ value ++ "\""


enum : EnumValue -> String
enum enumValue =
    "." ++ EnumValue.toString enumValue ++ "."


bool : Bool -> String
bool value =
    if value then
        ".T."

    else
        ".F."


int : Int -> String
int value =
    String.fromInt value


float : Float -> String
float value =
    let
        floatString =
            String.fromFloat value
    in
    if String.contains "." floatString then
        floatString

    else
        -- No decimal point, so must be an integer-valued float; add a
        -- trailing decimal point as required by the STEP standard
        floatString ++ "."


derivedValue : String
derivedValue =
    "*"


null : String
null =
    "$"


list : List String -> String
list attributeValues =
    "(" ++ String.join "," attributeValues ++ ")"


typedAttribute : TypeName -> String -> String
typedAttribute typeName attributeValue =
    TypeName.toString typeName ++ "(" ++ attributeValue ++ ")"


id : Int -> String
id value =
    "#" ++ String.fromInt value
