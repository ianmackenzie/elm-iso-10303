module StepFile.Attribute
    exposing
        ( Attribute
        , binary
        , binaryAs
        , default
        , enum
        , enumAs
        , float
        , floatAs
        , int
        , intAs
        , list
        , listAs
        , null
        , referenceTo
        , string
        , stringAs
        )

{-| Functions for constructing STEP attributes.

@docs Attribute


@docs referenceTo, default, null, int, float, string, enum, binary, list


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs intAs, floatAs, stringAs, enumAs, binaryAs, listAs

-}

import StepFile.Format as Format
import StepFile.Types as Types


{-| Represents a single attribute of a STEP entity, such as X coordinate value,
GUID string, or a reference to another entity.
-}
type alias Attribute =
    Types.Attribute


{-| Construct a reference to another STEP entity (will end up being encoded
using an integer ID in the resulting STEP file, e.g. `#123`).
-}
referenceTo : Types.Entity -> Attribute
referenceTo entity_ =
    Types.ReferenceTo entity_


{-| The special 'default value' attribute (`*` in the resulting STEP file).
-}
default : Attribute
default =
    Types.DefaultAttribute


{-| The special 'null value' attribute (`$` in the resulting STEP file).
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
