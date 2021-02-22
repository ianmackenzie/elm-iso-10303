module Step.Format exposing (derivedValue, null, bool, int, float, string, id, enum, binaryData, list, typedAttribute)

{-| Low-level attribute formatting functionality. Usually you will want to use
the functions in the [`Step.Encode`](Step-Encode) module instead.

@docs derivedValue, null, bool, int, float, string, id, enum, binaryData, list, typedAttribute

-}

import Bytes
import Bytes.Encode
import Step.Bytes
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.String
import Step.TypeName as TypeName exposing (TypeName)


{-| Format a string by wrapping it in single quotation marks. Unicode characters
will be properly escaped according to the (weird, custom) method specified in
the STEP standard; for example, "see § 4.1" will be encoded as `'see \X\A7 4.1'`.
-}
string : String -> String
string value =
    "'" ++ Step.String.encode value ++ "'"


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
    "\"" ++ Step.Bytes.encode (Bytes.Encode.encode bytesEncoder) ++ "\""


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
