module Step.String exposing (encode, decode)

{-| Low-level functionality for encoding and decoding strings as specified in
the STEP standard. In most case you will want to use [`Step.Decode.string`](Step-Decode#string)
and [`Step.Encode.string`](Step-Encode#string) instead of using this module
directly.

@docs encode, decode

-}

import Bitwise
import Regex exposing (Regex)
import Step.Hex as Hex


type EscapeContext
    = NoEscapeContext
    | EscapeX2
    | EscapeX4


type alias StringFoldState =
    { encodedChunks : List String
    , escapeContext : EscapeContext
    }


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


{-| Encode a string using the (weird, custom) method specified in the STEP
standard:

    Step.String.encode "see § 4.1"
    --> "see \X\A7 4.1"

Note that the leading and trailing single quotation marks (used when writing
strings to a STEP file) are _not_ included in the result.

-}
encode : String -> String
encode value =
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
        String.concat foldResult.encodedChunks

    else
        -- Optimization for plain ASCII strings
        value


{-| Decode a STEP-encoded string.
-}
decode : String -> String
decode encodedString =
    let
        string1 =
            if String.contains "''" encodedString then
                String.replace "''" "'" encodedString

            else
                encodedString
    in
    if String.contains "\\" string1 then
        string1
            |> Regex.replace xRegex replaceX
            |> Regex.replace x2Regex replaceX2
            |> Regex.replace x4Regex replaceX4
            |> String.replace "\\\\" "\\"

    else
        string1


xRegex : Regex
xRegex =
    Regex.fromString "\\\\X\\\\[0-9A-F]{2}"
        |> Maybe.withDefault Regex.never


x2Regex : Regex
x2Regex =
    Regex.fromString "\\\\X2\\\\[0-9A-F]+\\\\X0\\\\"
        |> Maybe.withDefault Regex.never


x4Regex : Regex
x4Regex =
    Regex.fromString "\\\\X4\\\\[0-9A-F]+\\\\X0\\\\"
        |> Maybe.withDefault Regex.never


replaceHexString : Int -> String -> String
replaceHexString chunkSize hexString =
    let
        foldResult =
            String.foldr
                (\hexCharacter { unicodeCharacters, index, current, multiplier } ->
                    let
                        updated =
                            current + multiplier * Hex.charToInt hexCharacter

                        nextIndex =
                            index + 1
                    in
                    if nextIndex < chunkSize then
                        { unicodeCharacters = unicodeCharacters
                        , index = nextIndex
                        , current = updated
                        , multiplier = 16 * multiplier
                        }

                    else
                        { unicodeCharacters = Char.fromCode updated :: unicodeCharacters
                        , index = 0
                        , current = 0
                        , multiplier = 1
                        }
                )
                { unicodeCharacters = [], index = 0, current = 0, multiplier = 1 }
                hexString
    in
    String.fromList foldResult.unicodeCharacters


replaceX : Regex.Match -> String
replaceX { match } =
    replaceHexString 2 (String.dropLeft 3 match)


replaceX2 : Regex.Match -> String
replaceX2 { match } =
    replaceHexString 4 (String.slice 4 -4 match)


replaceX4 : Regex.Match -> String
replaceX4 { match } =
    replaceHexString 8 (String.slice 4 -4 match)
