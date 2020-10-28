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


{-| Format a string by wrapping it in single quotation marks. Unicode characters
will be properly escaped according to the (weird, custom) method specified in
the STEP standard; for example, "see § 4.1" will be encoded as `'see \X\A7 4.1'`.
-}
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


{-| Format a string (currently assumed to be already hex-encoded according to
the STEP standard) by wrapping it in double quotation marks.
-}
binary : String -> String
binary value =
    "\"" ++ value ++ "\""


{-| Format an enum value as a capitalized string with leading and trailing
periods, for example `.METRE.`.
-}
enum : EnumValue -> String
enum enumValue =
    "." ++ EnumValue.toString enumValue ++ "."


{-| Format a boolean value as either `.T.` or `.F.`.
-}
bool : Bool -> String
bool value =
    if value then
        ".T."

    else
        ".F."


{-| Format an integer value. (This is really just an alias for Elm's built-in
`String.fromInt`.)
-}
int : Int -> String
int value =
    String.fromInt value


{-| Format a floating-point value. This is almost the same as Elm's built-in
`String.fromFloat` but (as required by the STEP standard) will ensure that the
resulting string has a trailing decimal place if one is not otherwise required.
For example, the value 3 will be formatted as `3.` instead of just `3`.
-}
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


{-| The special 'derived value' string `*`.
-}
derivedValue : String
derivedValue =
    "*"


{-| The special 'null value' string `$`.
-}
null : String
null =
    "$"


{-| Given a list strings (assumed to be formatted attribute values produced by
other functions in this module), join them using a comma and enclose them in
parentheses, for example `(#1,#2,#3)` for a list of entity references.
-}
list : List String -> String
list attributeValues =
    "(" ++ String.join "," attributeValues ++ ")"


{-| Format a typed attribute by surrounding an existing attribute (given as a
string) in parentheses and prepending the given type name, for example
`SOME_TYPE_NAME(123)` for a typed integer attribute.
-}
typedAttribute : TypeName -> String -> String
typedAttribute typeName attributeValue =
    TypeName.toString typeName ++ "(" ++ attributeValue ++ ")"


{-| Format a STEP integer ID as (for example) `#123`.
-}
id : Int -> String
id value =
    "#" ++ String.fromInt value
