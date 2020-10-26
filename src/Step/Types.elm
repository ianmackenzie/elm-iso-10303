module Step.Types exposing (..)

{-| The internal STEP types used during both encoding and decoding.
-}

import Dict exposing (Dict)
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.TypeName as TypeName exposing (TypeName)


{-| An attribute value string such as "1", "2.", "'some string'", ".STEEL." or
"#34".
-}
type AttributeValue
    = AttributeValue String


{-| A single STEP entity.
-}
type Entity
    = Entity TypeName (List Attribute)
    | ComplexEntity (List ( TypeName, List Attribute ))


{-| An attribute of a STEP entity.
-}
type Attribute
    = DerivedAttribute
    | NullAttribute
    | BoolAttribute Bool
    | IntAttribute Int
    | FloatAttribute Float
    | StringAttribute String
    | BinaryAttribute String
    | EnumAttribute EnumValue
    | ReferenceTo Entity
    | TypedAttribute TypeName Attribute
    | AttributeList (List Attribute)


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
