module StepFile.Entity
    exposing
        ( Entity
        , attributes
        , ofType
        , typeName
        )

{-| An `Entity` represents a single entity stored in the data section of a STEP
file. An entity may be a point, a curve, a part, an assembly, or even an entire
building. Every entity has a type and a list of attributes (which themselves be
references to other entities).

@docs Entity, ofType


# Properties

@docs typeName, attributes

-}

import StepFile.Attribute as Attribute exposing (Attribute)
import StepFile.Format as Format
import StepFile.Types as Types


{-| -}
type alias Entity =
    Types.Entity


{-| Construct a single STEP entity with the given type and attributes. The type
name will be capitalized if necessary.
-}
ofType : String -> List Attribute -> Entity
ofType givenTypeName givenAttributes =
    Types.Entity (Format.typeName givenTypeName) givenAttributes


{-| Get the type name of an entity. This will always be all caps, for example
`"IFCWALL"` or `"PRODUCT_CONTEXT"`.
-}
typeName : Entity -> String
typeName (Types.Entity (Types.TypeName string) _) =
    string


{-| Check if an entity has the given type. It is preferred to use

    Entity.hasType someType someEntity

over

    Entity.typeName someEntity == someType

since in the first case the comparison is done in a case-insensitive way while
in the second case will only work if you ensure that the `someType` string is in
all caps.

-}
hasType : String -> Entity -> Bool
hasType givenTypeName =
    let
        (Types.TypeName formattedTypeName) =
            Format.typeName givenTypeName
    in
    \entity -> typeName entity == formattedTypeName


{-| Get the attributes of an entity.
-}
attributes : Entity -> List Attribute
attributes (Types.Entity _ attributes_) =
    attributes_
