module Step.EntityId exposing (EntityId, toInt)

{-|

@docs EntityId, toInt

-}

import Step.Internal as Internal


{-| The integer ID for a STEP entity.
-}
type alias EntityId =
    Internal.EntityId


{-| Get the integer value of an entity ID.
-}
toInt : EntityId -> Int
toInt entityId =
    let
        (Internal.EntityId id) =
            entityId
    in
    id
