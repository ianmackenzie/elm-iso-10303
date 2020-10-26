module Step.Types exposing (..)

{-| The internal STEP types used during both encoding and decoding.
-}

import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.TypeName as TypeName exposing (TypeName)


{-| An attribute value string such as "1", "2.", "'some string'", ".STEEL." or
"#34".
-}
type AttributeValue
    = AttributeValue String


{-| An attribute that has been parsed but not yet resolved (may be an
unresolved entity reference).
-}
type ParsedAttribute
    = ParsedDerivedAttribute
    | ParsedNullAttribute
    | ParsedBoolAttribute Bool
    | ParsedIntAttribute Int
    | ParsedFloatAttribute Float
    | ParsedStringAttribute String
    | ParsedBinaryAttribute String
    | ParsedEnumAttribute EnumValue
    | ParsedReference Int
    | ParsedTypedAttribute TypeName ParsedAttribute
    | ParsedAttributeList (List ParsedAttribute)


{-| An entity that has been parsed but not yet resolved (whose attributes may
contain unresolved entity references).
-}
type ParsedEntity
    = ParsedSimpleEntity TypeName (List ParsedAttribute)
    | ParsedComplexEntity (List ( TypeName, List ParsedAttribute ))
