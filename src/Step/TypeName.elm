module Step.TypeName exposing (TypeName, fromString, toString)

{-| -}


{-| A capitalized type name like "IFCWALL".
-}
type TypeName
    = TypeName String


fromString : String -> TypeName
fromString string =
    TypeName (String.toUpper string)


toString : TypeName -> String
toString (TypeName string) =
    string
