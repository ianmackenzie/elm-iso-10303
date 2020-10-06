module Step.TypeName exposing (TypeName, fromString, toString)

import Step.Types as Types


type alias TypeName =
    Types.TypeName


fromString : String -> TypeName
fromString string =
    Types.TypeName (String.toUpper string)


toString : TypeName -> String
toString (Types.TypeName string) =
    string
