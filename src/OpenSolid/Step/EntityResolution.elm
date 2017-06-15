module OpenSolid.Step.EntityResolution exposing (Error(..), resolve)

import Dict exposing (Dict)
import OpenSolid.Step.Types as Types
import Set exposing (Set)


type Error
    = NonexistentEntity Int


type alias EntityResolution =
    { parsedMap : Dict Int Types.ParsedEntity
    , resolvedMap : Dict Int Types.Entity
    }


init : List ( Int, Types.ParsedEntity ) -> EntityResolution
init parsedEntityInstances =
    { parsedMap = Dict.fromList parsedEntityInstances
    , resolvedMap = Dict.empty
    }


store : Int -> Types.Entity -> EntityResolution -> EntityResolution
store id entity entityResolution =
    { entityResolution
        | resolvedMap = Dict.insert id entity entityResolution.resolvedMap
    }


addEntity : Int -> Types.ParsedEntity -> EntityResolution -> Set Int -> Result Error ( Types.Entity, EntityResolution )
addEntity id { typeName, parsedAttributes } entityResolution parentIds =
    case Dict.get id entityResolution.resolvedMap of
        Just entity ->
            Ok ( entity, entityResolution )

        Nothing ->
            addAttributeList parsedAttributes entityResolution parentIds
                |> Result.map
                    (\( attributeList, resolutionWithAttributes ) ->
                        let
                            entity =
                                Types.Entity typeName attributeList

                            updatedResolution =
                                store id entity resolutionWithAttributes
                        in
                        ( entity, updatedResolution )
                    )


addAttribute : Types.ParsedAttribute -> EntityResolution -> Set Int -> Result Error ( Types.Attribute, EntityResolution )
addAttribute parsedAttribute entityResolution parentIds =
    case parsedAttribute of
        Types.ParsedDefaultAttribute ->
            Ok ( Types.DefaultAttribute, entityResolution )

        Types.ParsedNullAttribute ->
            Ok ( Types.NullAttribute, entityResolution )

        Types.ParsedBoolAttribute value ->
            Ok ( Types.BoolAttribute value, entityResolution )

        Types.ParsedIntAttribute value ->
            Ok ( Types.IntAttribute value, entityResolution )

        Types.ParsedFloatAttribute value ->
            Ok ( Types.FloatAttribute value, entityResolution )

        Types.ParsedStringAttribute value ->
            Ok ( Types.StringAttribute value, entityResolution )

        Types.ParsedBinaryAttribute value ->
            Ok ( Types.BinaryAttribute value, entityResolution )

        Types.ParsedEnumAttribute name ->
            Ok ( Types.EnumAttribute name, entityResolution )

        Types.ParsedReference id ->
            case Dict.get id entityResolution.resolvedMap of
                Just entity ->
                    -- Found an already-resolved entity
                    Ok ( Types.ReferenceTo entity, entityResolution )

                Nothing ->
                    case Dict.get id entityResolution.parsedMap of
                        Just parsedEntity ->
                            -- Found a parsed but not yet resolved entity
                            if Set.member id parentIds then
                                -- Referenced entity is a parent of the given
                                -- entity (or equal to it)
                                Ok
                                    ( Types.CircularReference id
                                    , entityResolution
                                    )
                            else
                                -- Referenced entity is not a parent of (or
                                -- equal to) the current entity, so recursively
                                -- add it
                                addEntity id
                                    parsedEntity
                                    entityResolution
                                    (Set.insert id parentIds)
                                    |> Result.map
                                        (\( entity, updatedResolution ) ->
                                            ( Types.ReferenceTo entity
                                            , updatedResolution
                                            )
                                        )

                        -- Error - no entity found with the given ID
                        Nothing ->
                            Err (NonexistentEntity id)

        Types.ParsedTypedAttribute name nestedParsedAttribute ->
            addAttribute nestedParsedAttribute entityResolution parentIds
                |> Result.map
                    (\( nestedAttribute, updatedResolution ) ->
                        ( Types.TypedAttribute name nestedAttribute
                        , updatedResolution
                        )
                    )

        Types.ParsedAttributeList parsedAttributeList ->
            addAttributeList parsedAttributeList entityResolution parentIds
                |> Result.map
                    (\( attributeList, updatedResolution ) ->
                        ( Types.AttributeList attributeList
                        , updatedResolution
                        )
                    )


addAttributeList : List Types.ParsedAttribute -> EntityResolution -> Set Int -> Result Error ( List Types.Attribute, EntityResolution )
addAttributeList parsedAttributeList entityResolution parentIds =
    addAttributeListHelp parsedAttributeList [] entityResolution parentIds
        |> Result.map (Tuple.mapFirst List.reverse)


addAttributeListHelp : List Types.ParsedAttribute -> List Types.Attribute -> EntityResolution -> Set Int -> Result Error ( List Types.Attribute, EntityResolution )
addAttributeListHelp parsedAttributeList reversedAttributeList entityResolution parentIds =
    case parsedAttributeList of
        [] ->
            Ok ( reversedAttributeList, entityResolution )

        first :: rest ->
            let
                addResult =
                    addAttribute first entityResolution parentIds
            in
            case addResult of
                Ok ( attribute, updatedResolution ) ->
                    addAttributeListHelp rest
                        (attribute :: reversedAttributeList)
                        updatedResolution
                        parentIds

                Err error ->
                    Err error


addEntities : List ( Int, Types.ParsedEntity ) -> EntityResolution -> Result Error EntityResolution
addEntities parsedEntityInstances entityResolution =
    case parsedEntityInstances of
        [] ->
            Ok entityResolution

        ( id, parsedEntity ) :: rest ->
            let
                parentIds =
                    Set.singleton id

                addResult =
                    addEntity id parsedEntity entityResolution parentIds
                        |> Result.map Tuple.second
            in
            case addResult of
                Ok updatedResolution ->
                    addEntities rest updatedResolution

                Err _ ->
                    addResult


resolve : List ( Int, Types.ParsedEntity ) -> Result Error (Dict Int Types.Entity)
resolve parsedEntityInstances =
    let
        entityResolution =
            init parsedEntityInstances
    in
    addEntities parsedEntityInstances entityResolution
        |> Result.map .resolvedMap
