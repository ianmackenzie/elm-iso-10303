module Step.EnumName exposing (EnumName, fromString, toString)

{-| -}


{-| A capitalized enum name like "STEEL", with no leading or trailing periods.
-}
type EnumName
    = EnumName String


fromString : String -> EnumName
fromString string =
    EnumName (string |> String.toUpper |> String.replace "." "")


toString : EnumName -> String
toString (EnumName string) =
    string
