module Step.Bytes exposing (decode, encode)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Step.Hex as Hex


byteValues : List Int -> List Int -> List Int
byteValues accumulated hexValues =
    case hexValues of
        first :: second :: rest ->
            byteValues ((first + 16 * second) :: accumulated) rest

        [ single ] ->
            single :: accumulated

        [] ->
            accumulated


decode : String -> Bytes
decode hexString =
    hexString
        -- ignore leading 'number of zero padding bits' value since elm/bytes
        -- only supports byte-aligned binary data
        |> String.dropLeft 1
        -- Normalize string to upper case
        |> String.toUpper
        -- Convert to list of character hex values
        |> String.toList
        |> List.map Hex.charToInt
        -- Combine pairs of (assumed hex) characters into byte values,
        -- starting from least significant
        |> List.reverse
        |> byteValues []
        -- Encode list of bytes as Bytes value
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


encode : Bytes -> String
encode bytes =
    let
        numBytes =
            Bytes.width bytes

        nibblesDecoder : Bytes.Decode.Decoder (List Int)
        nibblesDecoder =
            Bytes.Decode.loop ( numBytes, [] ) collectNibbles

        nibbles =
            Bytes.Decode.decode nibblesDecoder bytes |> Maybe.withDefault []
    in
    "0" ++ String.fromList (List.map Hex.intToChar nibbles)


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
