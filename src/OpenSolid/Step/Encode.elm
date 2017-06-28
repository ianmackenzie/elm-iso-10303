module OpenSolid.Step.Encode
    exposing
        ( binary
        , binaryAs
        , default
        , entity
        , enum
        , enumAs
        , float
        , floatAs
        , int
        , intAs
        , list
        , listAs
        , null
        , optional
        , referenceTo
        , string
        , stringAs
        )

{-| Functions for encoding data in STEP format.


# Entities

@docs entity


# Attributes

@docs referenceTo, default, null, int, float, string, enum, binary, list, optional


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs intAs, floatAs, stringAs, enumAs, binaryAs, listAs

-}

import OpenSolid.Step exposing (Attribute, Entity)
import OpenSolid.Step.Format as Format
import OpenSolid.Step.Types as Types


{-| Construct a single STEP entity from a type name and list of attributes.
-}
entity : String -> List Attribute -> Entity
entity typeName attributes =
    Types.Entity (Format.typeName typeName) attributes


{-| Construct a reference to another STEP entity.
-}
referenceTo : Entity -> Attribute
referenceTo entity =
    Types.ReferenceTo entity


{-| The special 'default value' attribute.
-}
default : Attribute
default =
    Types.DefaultAttribute


{-| The special 'null value' attribute.
-}
null : Attribute
null =
    Types.NullAttribute


{-| Construct a Boolean-valued attribute.

Boolean values are actually encoded as enumeration values `.T.` and `.F.`.

-}
bool : Bool -> Attribute
bool value =
    Types.BoolAttribute value


{-| Construct an integer-valued attribute.
-}
int : Int -> Attribute
int value =
    Types.IntAttribute value


{-| Construct a real-valued attribute.
-}
float : Float -> Attribute
float value =
    Types.FloatAttribute value


{-| Construct a string-valued attribute.
-}
string : String -> Attribute
string value =
    Types.StringAttribute value


{-| Construct an attribute that refers to an enumeration value defined in an
EXPRESS schema. Enumeration values are always encoded as all-caps with leading
and trailing periods, like `.STEEL.`; this function will capitalize and add
periods if necessary.
-}
enum : String -> Attribute
enum value =
    Types.EnumAttribute (Format.enumName value)


{-| Construct a binary-valued attribute. The provided string is assumed to
already be hex encoded as required by the STEP standard.
-}
binary : String -> Attribute
binary value =
    Types.BinaryAttribute value


{-| Construct an attribute which is itself a list of other attributes.
-}
list : List Attribute -> Attribute
list attributes =
    Types.AttributeList attributes


{-| Construct a type-tagged Boolean-valued attribute.
-}
boolAs : String -> Bool -> Attribute
boolAs typeName value =
    typedAttribute typeName (bool value)


{-| Construct a type-tagged integer-valued attribute.
-}
intAs : String -> Int -> Attribute
intAs typeName value =
    typedAttribute typeName (int value)


{-| Construct a type-tagged float-valued attribute.
-}
floatAs : String -> Float -> Attribute
floatAs typeName value =
    typedAttribute typeName (float value)


{-| Construct a type-tagged string-valued attribute.
-}
stringAs : String -> String -> Attribute
stringAs typeName value =
    typedAttribute typeName (string value)


{-| Construct a type-tagged enumeration attribute.
-}
enumAs : String -> String -> Attribute
enumAs typeName value =
    typedAttribute typeName (enum value)


{-| Construct a type-tagged binary-valued attribute.
-}
binaryAs : String -> String -> Attribute
binaryAs typeName value =
    typedAttribute typeName (binary value)


{-| Construct a type-tagged list attribute.
-}
listAs : String -> List Attribute -> Attribute
listAs typeName attributes =
    typedAttribute typeName (list attributes)


typedAttribute : String -> Attribute -> Attribute
typedAttribute typeName attribute =
    Types.TypedAttribute (Format.typeName typeName) attribute


{-| Construct an optional attribute from a `Maybe` value. If given value is
`Just value` then `value` will be encoded using the given encoder; otherwise
it will be encoded as `null`.

    Encode.optional Encode.int (Just 3)
    --> Encode.int 3

    Encode.optional Encode.int Nothing
    --> Encode.null

-}
optional : (a -> Attribute) -> Maybe a -> Attribute
optional valueEncoder maybe =
    case maybe of
        Just value ->
            valueEncoder value

        Nothing ->
            null
