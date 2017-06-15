module OpenSolid.Step.EntityStack
    exposing
        ( EntityStack
        , push
        , singleton
        )

import List.Extra as List
import Set exposing (Set)


type EntityStack
    = EntityStack (Set Int) (List Int)


singleton : Int -> EntityStack
singleton id =
    EntityStack (Set.singleton id) [ id ]


push : Int -> EntityStack -> Result (List Int) EntityStack
push id (EntityStack idSet idStack) =
    let
        updatedStack =
            id :: idStack
    in
    if Set.member id idSet then
        Err (updatedStack |> List.reverse |> List.dropWhile ((/=) id))
    else
        Ok (EntityStack (Set.insert id idSet) updatedStack)
