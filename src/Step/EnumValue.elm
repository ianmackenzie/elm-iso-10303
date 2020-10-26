module Step.EnumValue exposing (EnumValue, fromString, toString)

{-|

@docs EnumValue, fromString, toString

-}


{-| A capitalized enum value such as "STEEL" or "METRE".
-}
type EnumValue
    = EnumValue String


{-| Construct an enum value from a string. The string will be capitalized and
will have periods removed.
-}
fromString : String -> EnumValue
fromString string =
    EnumValue (string |> String.toUpper |> String.replace "." "")


{-| Convert an enum value to a string. The result will always be capitalized
and will not have a leading/trailing periods (enum values are encoded in STEP
files using leading and trailing periods, but those periods are considered to be
an encoding detail and not part of the enum value).
-}
toString : EnumValue -> String
toString (EnumValue string) =
    string
