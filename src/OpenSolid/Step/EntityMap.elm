module OpenSolid.Step.EntityMap
    exposing
        ( EntityMap
        , add
        , empty
        , toList
        )

import Dict exposing (Dict)


type EntityMap
    = EntityMap Int (Dict String Int)


empty : EntityMap
empty =
    EntityMap 1 Dict.empty


add : String -> EntityMap -> ( Int, EntityMap )
add entityString ((EntityMap nextId idMap) as entityMap) =
    case Dict.get entityString idMap of
        Just id ->
            ( id, entityMap )

        Nothing ->
            ( nextId
            , EntityMap (nextId + 1) (Dict.insert entityString nextId idMap)
            )


toList : EntityMap -> List ( Int, String )
toList (EntityMap _ idMap) =
    Dict.toList idMap
        |> List.map (\( string, id ) -> ( id, string ))
        |> List.sortBy Tuple.first
