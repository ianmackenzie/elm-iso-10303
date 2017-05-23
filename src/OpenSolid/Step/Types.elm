module OpenSolid.Step.Types exposing (..)

{-| The core STEP types used during both encoding and decoding.
-}


{-| An attribute value string such as "1", "2.", "'some string'", ".STEEL." or
"#34".
-}
type AttributeValue
    = AttributeValue String


{-| A capitalized type name like "IFCWALL".
-}
type TypeName
    = TypeName String


{-| A capitalized enum name like "STEEL", with no leading or trailing periods.
-}
type EnumName
    = EnumName String


{-| A single STEP entity.
-}
type Entity
    = Entity TypeName (List Attribute)


{-| An attribute of a STEP entity.
-}
type Attribute
    = DefaultAttribute
    | NullAttribute
    | BoolAttribute Bool
    | IntAttribute Int
    | FloatAttribute Float
    | StringAttribute String
    | BinaryAttribute String
    | EnumAttribute EnumName
    | ReferenceTo Entity
    | TypedAttribute TypeName Attribute
    | AttributeList (List Attribute)


type ParsedAttribute
    = ParsedDefaultAttribute
    | ParsedNullAttribute
    | ParsedBoolAttribute Bool
    | ParsedIntAttribute Int
    | ParsedFloatAttribute Float
    | ParsedStringAttribute String
    | ParsedBinaryAttribute String
    | ParsedEnumAttribute EnumName
    | ParsedReference Int
    | ParsedTypedAttribute TypeName ParsedAttribute
    | ParsedAttributeList (List ParsedAttribute)


type alias ParsedEntity =
    { typeName : TypeName
    , parsedAttributes : List ParsedAttribute
    }
