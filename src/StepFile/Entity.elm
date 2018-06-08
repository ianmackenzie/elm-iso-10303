module StepFile.Entity
    exposing
        ( attributes
        , hasType
        , typeName
        )

{-| Accessors for `Entity` values. Usually you will not need this module,
instead using the `Decode` module to decode entities directly, but it may come
in handy in weird cases.

@docs typeName, hasType, attributes

-}

import StepFile exposing (Attribute, Entity)
import StepFile.Format as Format
import StepFile.Types as Types


{-| Get the type of an entity. This will always be all caps, for example
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


{-| Get the attributes of an entity, in the order they are given in the STEP
file. About the only thing you can then do with each attribute is attempt to
decode it to some value of type `a` using `Decode.run` with a
`Decoder Attribute a`.
-}
attributes : Entity -> List Attribute
attributes (Types.Entity _ attributes_) =
    attributes_
