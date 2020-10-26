module Step.EnumValue exposing (EnumValue, fromString, toString)

{-| -}


{-| A capitalized enum name like "STEEL", with no leading or trailing periods.
-}
type EnumValue
    = EnumValue String


fromString : String -> EnumValue
fromString string =
    EnumValue (string |> String.toUpper |> String.replace "." "")


toString : EnumValue -> String
toString (EnumValue string) =
    string
