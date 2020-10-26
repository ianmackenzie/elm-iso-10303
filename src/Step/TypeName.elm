module Step.TypeName exposing (TypeName, fromString, toString)

{-|

@docs TypeName, fromString, toString

-}


{-| A capitalized type name like "IFCWALL".
-}
type TypeName
    = TypeName String


{-| Construct a type name from a string. This wil capitalize the string.
-}
fromString : String -> TypeName
fromString string =
    TypeName (String.toUpper string)


{-| Convert a type name to a string. The result will always be capitalized.
-}
toString : TypeName -> String
toString (TypeName string) =
    string
