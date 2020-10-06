module Step.EnumName exposing (EnumName, fromString, toString)

import Step.Types as Types


type alias EnumName =
    Types.EnumName


fromString : String -> EnumName
fromString string =
    Types.EnumName (string |> String.toUpper |> String.replace "." "")


toString : EnumName -> String
toString (Types.EnumName string) =
    "." ++ string ++ "."
