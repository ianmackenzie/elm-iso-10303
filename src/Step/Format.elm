module Step.Format exposing (derivedValue, null, bool, int, float, string, id, enum, binaryData, list, typedAttribute)

{-| Low-level attribute formatting functionality. Usually you will want to use
the functions in the [`Step.Encode`](Step-Encode) module instead.

@docs derivedValue, null, bool, int, float, string, id, enum, binaryData, list, typedAttribute

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Char
import Regex exposing (Regex)
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.Hex as Hex
import Step.TypeName as TypeName exposing (TypeName)


type EscapeContext
    = NoEscapeContext
    | EscapeX2
    | EscapeX4


type alias StringFoldState =
    { encodedChunks : List String
    , escapeContext : EscapeContext
    }


addAscii : String -> StringFoldState -> StringFoldState
addAscii value { encodedChunks, escapeContext } =
    case escapeContext of
        NoEscapeContext ->
            { encodedChunks = value :: encodedChunks
            , escapeContext = NoEscapeContext
            }

        EscapeX2 ->
            { encodedChunks = value :: "\\X2\\" :: encodedChunks
            , escapeContext = NoEscapeContext
            }

        EscapeX4 ->
            { encodedChunks = value :: "\\X4\\" :: encodedChunks
            , escapeContext = NoEscapeContext
            }


addX : Int -> StringFoldState -> StringFoldState
addX codePoint { encodedChunks, escapeContext } =
    let
        newChunk =
            hexEncode 2 codePoint
    in
    case escapeContext of
        NoEscapeContext ->
            { encodedChunks = "\\X\\" :: newChunk :: encodedChunks
            , escapeContext = NoEscapeContext
            }

        EscapeX2 ->
            { encodedChunks = "\\X\\" :: newChunk :: "\\X2\\" :: encodedChunks
            , escapeContext = NoEscapeContext
            }

        EscapeX4 ->
            { encodedChunks = "\\X\\" :: newChunk :: "\\X4\\" :: encodedChunks
            , escapeContext = NoEscapeContext
            }


addX2 : Int -> StringFoldState -> StringFoldState
addX2 codePoint { encodedChunks, escapeContext } =
    let
        newChunk =
            hexEncode 4 codePoint
    in
    case escapeContext of
        NoEscapeContext ->
            { encodedChunks = newChunk :: "\\X0\\" :: encodedChunks
            , escapeContext = EscapeX2
            }

        EscapeX2 ->
            { encodedChunks = newChunk :: encodedChunks
            , escapeContext = EscapeX2
            }

        EscapeX4 ->
            { encodedChunks = newChunk :: "\\X0\\\\X4\\" :: encodedChunks
            , escapeContext = EscapeX2
            }


addX4 : Int -> StringFoldState -> StringFoldState
addX4 codePoint { encodedChunks, escapeContext } =
    let
        newChunk =
            hexEncode 8 codePoint
    in
    case escapeContext of
        NoEscapeContext ->
            { encodedChunks = newChunk :: "\\X0\\" :: encodedChunks
            , escapeContext = EscapeX4
            }

        EscapeX2 ->
            { encodedChunks = newChunk :: "\\X0\\\\X2\\" :: encodedChunks
            , escapeContext = EscapeX4
            }

        EscapeX4 ->
            { encodedChunks = newChunk :: encodedChunks
            , escapeContext = EscapeX4
            }


nonAsciiCharacter : Regex
nonAsciiCharacter =
    Regex.fromString "[^ -~]" |> Maybe.withDefault Regex.never


{-| Format a string by wrapping it in single quotation marks. Unicode characters
will be properly escaped according to the (weird, custom) method specified in
the STEP standard; for example, "see § 4.1" will be encoded as `'see \X\A7 4.1'`.
-}
string : String -> String
string value =
    if Regex.contains nonAsciiCharacter value then
        let
            foldResult =
                String.foldr
                    (\character foldState ->
                        if character == '\'' then
                            addAscii "''" foldState

                        else if character == '\\' then
                            addAscii "\\" foldState

                        else
                            let
                                codePoint =
                                    Char.toCode character
                            in
                            if codePoint >= 0 && codePoint <= 0x1F then
                                addX codePoint foldState

                            else if codePoint >= 0x20 && codePoint <= 0x7E then
                                addAscii (String.fromChar character) foldState

                            else if codePoint >= 0x7F && codePoint <= 0xFF then
                                addX codePoint foldState

                            else if codePoint >= 0x0100 && codePoint <= 0xFFFF then
                                addX2 codePoint foldState

                            else if codePoint >= 0x00010000 && codePoint <= 0x0010FFFF then
                                addX4 codePoint foldState

                            else
                                -- Shouldn't happen, outside Unicode range
                                foldState
                    )
                    { encodedChunks = []
                    , escapeContext = NoEscapeContext
                    }
                    value
                    -- Finalize any dangling \X2\ or \X4\ group
                    |> addAscii ""
        in
        "'" ++ String.concat foldResult.encodedChunks ++ "'"

    else
        -- Optimization for plain ASCII strings
        "'" ++ value ++ "'"


apostropheCodePoint : Int
apostropheCodePoint =
    Char.toCode '\''


backslashCodePoint : Int
backslashCodePoint =
    Char.toCode '\\'


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


collectNibbles : ( Int, List Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Int ) (List Int))
collectNibbles ( count, accumulated ) =
    if count <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse accumulated))

    else
        Bytes.Decode.unsignedInt8
            |> Bytes.Decode.map
                (\byte ->
                    let
                        lowNibble =
                            byte |> modBy 16

                        highNibble =
                            byte // 16
                    in
                    Bytes.Decode.Loop ( count - 1, lowNibble :: highNibble :: accumulated )
                )


{-| Format binary data as a hex-encoded string according to the STEP
standard and wrap it in double quotation marks.

Note that since Elm only supports byte-aligned binary data, there is no way to
create (for example) a blob of binary data exactly 5 bits in size even though
that is supported by the STEP standard. As a result, the encoded string will
always start with 0 (since that character is otherwise used to indicate a
number of padding zero bits).

-}
binaryData : Bytes.Encode.Encoder -> String
binaryData bytesEncoder =
    let
        bytes =
            Bytes.Encode.encode bytesEncoder

        numBytes =
            Bytes.width bytes

        nibblesDecoder : Bytes.Decode.Decoder (List Int)
        nibblesDecoder =
            Bytes.Decode.loop ( numBytes, [] ) collectNibbles

        nibbles =
            Bytes.Decode.decode nibblesDecoder bytes |> Maybe.withDefault []
    in
    "\"0" ++ String.fromList (List.map Hex.intToChar nibbles) ++ "\""


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
