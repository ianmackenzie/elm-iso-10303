module OpenSolid.Step.Types exposing (..)

{-| The internal STEP types used during both encoding and decoding.
-}

import Date exposing (Date)
import Dict exposing (Dict)


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


{-| An attribute that has been parsed but not yet resolved (may be an
unresolved entity reference).
-}
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


{-| An entity that has been parsed but not yet resolved (whose attributes may
contain unresolved entity references).
-}
type alias ParsedEntity =
    { typeName : TypeName
    , parsedAttributes : List ParsedAttribute
    }


type Decoder i a
    = Decoder (i -> Result String a)


type File
    = File
        { header : Header
        , entities : Dict Int Entity
        , contents : String
        }


type Header
    = Header
        { fileDescription : List String
        , fileName : String
        , timeStamp : Date
        , author : List String
        , organization : List String
        , preprocessorVersion : String
        , originatingSystem : String
        , authorization : String
        , schemaIdentifiers : List String
        }
