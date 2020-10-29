module Step.Internal exposing (..)

{-| The internal STEP types used during both encoding and decoding.
-}

import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types exposing (Entity, Header)


{-| Represents an entire STEP file composed of a header and a list of entities.
The only way to extract data from a `File` is by using a [decoder](Step-Decode#Decoder).
-}
type File
    = File
        { header : Header
        , allEntities : List Entity
        , topLevelEntities : List Entity
        }


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
