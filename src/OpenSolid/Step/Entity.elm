module OpenSolid.Step.Entity exposing (attributes, typeName)

{-| Accessors for `Entity` values. Usually you will not need this module,
instead using the `Decode` module to decode entities directly, but it may come
in handy in weird cases.

@docs typeName, attributes

-}

import OpenSolid.Step exposing (Attribute, Entity)
import OpenSolid.Step.Types as Types


{-| Get the type of an entity. This will always be capitalized, for example
`"IFCWALL"` or `"PRODUCT_CONTEXT"`.
-}
typeName : Entity -> String
typeName (Types.Entity (Types.TypeName string) _) =
    string


{-| Get the attributes of an entity, in the order they are given in the STEP
file. About the only thing you can then do with each attribute is attempt to
decode it to some value of type `a` using `Decode.run` with a
`Decoder Attribute a`.
-}
attributes : Entity -> List Attribute
attributes (Types.Entity _ attributes_) =
    attributes_
