module Step.EntityResolution exposing (Error(..), resolve)

import Dict exposing (Dict)
import Set exposing (Set)
import Step.EntityStack as EntityStack exposing (EntityStack)
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.Internal exposing (ParsedAttribute(..), ParsedEntity(..))
import Step.TypeName as TypeName exposing (TypeName)
import Step.Types as Types exposing (Attribute, Entity)


type Error
    = NonexistentEntity Int
    | CircularReference (List Int)


type alias EntityResolution =
    { parsedMap : Dict Int ParsedEntity
    , resolvedMap : Dict Int Entity
    , referencedIds : Set Int
    }


init : List ( Int, ParsedEntity ) -> EntityResolution
init parsedEntityInstances =
    { parsedMap = Dict.fromList parsedEntityInstances
    , resolvedMap = Dict.empty
    , referencedIds = Set.empty
    }


store : Int -> Entity -> EntityResolution -> EntityResolution
store id entity entityResolution =
    { entityResolution
        | resolvedMap = Dict.insert id entity entityResolution.resolvedMap
    }


addReference : Int -> EntityResolution -> EntityResolution
addReference id entityResolution =
    { entityResolution
        | referencedIds = Set.insert id entityResolution.referencedIds
    }


addEntity : Int -> ParsedEntity -> EntityResolution -> EntityStack -> Result Error ( Entity, EntityResolution )
addEntity id parsedEntity entityResolution entityStack =
    case Dict.get id entityResolution.resolvedMap of
        Just entity ->
            Ok ( entity, entityResolution )

        Nothing ->
            case parsedEntity of
                ParsedSimpleEntity typeName parsedAttributes ->
                    addAttributeList parsedAttributes entityResolution entityStack
                        |> Result.map
                            (\( attributes, resolutionWithAttributes ) ->
                                let
                                    entity =
                                        Types.SimpleEntity typeName attributes

                                    updatedResolution =
                                        store id entity resolutionWithAttributes
                                in
                                ( entity, updatedResolution )
                            )

                ParsedComplexEntity parsedSimpleEntities ->
                    addSimpleEntities parsedSimpleEntities entityResolution entityStack []
                        |> Result.map
                            (\( entityRecords, resolutionWithSimpleEntities ) ->
                                let
                                    entity =
                                        Types.ComplexEntity entityRecords

                                    updatedResolution =
                                        store id entity resolutionWithSimpleEntities
                                in
                                ( entity, updatedResolution )
                            )


addSimpleEntities :
    List ( TypeName, List ParsedAttribute )
    -> EntityResolution
    -> EntityStack
    -> List ( TypeName, List Attribute )
    -> Result Error ( List ( TypeName, List Attribute ), EntityResolution )
addSimpleEntities parsedSimpleEntities entityResolution entityStack accumulated =
    case parsedSimpleEntities of
        ( typeName, parsedAttributes ) :: rest ->
            case addAttributeList parsedAttributes entityResolution entityStack of
                Ok ( attributes, resolutionWithAttributes ) ->
                    addSimpleEntities rest resolutionWithAttributes entityStack <|
                        (( typeName, attributes ) :: accumulated)

                Err error ->
                    Err error

        [] ->
            Ok ( List.reverse accumulated, entityResolution )


addAttribute : ParsedAttribute -> EntityResolution -> EntityStack -> Result Error ( Attribute, EntityResolution )
addAttribute parsedAttribute entityResolution entityStack =
    case parsedAttribute of
        ParsedDerivedAttribute ->
            Ok ( Types.DerivedValue, entityResolution )

        ParsedNullAttribute ->
            Ok ( Types.NullAttribute, entityResolution )

        ParsedBoolAttribute value ->
            Ok ( Types.BoolAttribute value, entityResolution )

        ParsedIntAttribute value ->
            Ok ( Types.IntAttribute value, entityResolution )

        ParsedFloatAttribute value ->
            Ok ( Types.FloatAttribute value, entityResolution )

        ParsedStringAttribute value ->
            Ok ( Types.StringAttribute value, entityResolution )

        ParsedBytesAttribute value ->
            Ok ( Types.BytesAttribute value, entityResolution )

        ParsedEnumAttribute name ->
            Ok ( Types.EnumAttribute name, entityResolution )

        ParsedReference id ->
            case Dict.get id entityResolution.resolvedMap of
                Just entity ->
                    -- Found an already-resolved entity
                    Ok ( Types.ReferenceTo entity, addReference id entityResolution )

                Nothing ->
                    case Dict.get id entityResolution.parsedMap of
                        Just parsedEntity ->
                            -- Found a parsed but not yet resolved entity
                            case EntityStack.push id entityStack of
                                Ok updatedStack ->
                                    -- Referenced entity is not an ancestor of
                                    -- (or equal to) the current entity, so
                                    -- recursively add it
                                    addEntity id
                                        parsedEntity
                                        entityResolution
                                        updatedStack
                                        |> Result.map
                                            (\( entity, updatedResolution ) ->
                                                ( Types.ReferenceTo entity
                                                , addReference id updatedResolution
                                                )
                                            )

                                Err chain ->
                                    -- Referenced entity is an ancestor of (or
                                    -- equal to) the given entity
                                    Err (CircularReference chain)

                        -- Error - no entity found with the given ID
                        Nothing ->
                            Err (NonexistentEntity id)

        ParsedTypedAttribute name nestedParsedAttribute ->
            addAttribute nestedParsedAttribute entityResolution entityStack
                |> Result.map
                    (\( nestedAttribute, updatedResolution ) ->
                        ( Types.TypedAttribute name nestedAttribute
                        , updatedResolution
                        )
                    )

        ParsedAttributeList parsedAttributeList ->
            addAttributeList parsedAttributeList entityResolution entityStack
                |> Result.map
                    (\( attributeList, updatedResolution ) ->
                        ( Types.AttributeList attributeList
                        , updatedResolution
                        )
                    )


addAttributeList : List ParsedAttribute -> EntityResolution -> EntityStack -> Result Error ( List Attribute, EntityResolution )
addAttributeList parsedAttributeList entityResolution entityStack =
    addAttributeListHelp parsedAttributeList [] entityResolution entityStack
        |> Result.map (Tuple.mapFirst List.reverse)


addAttributeListHelp : List ParsedAttribute -> List Attribute -> EntityResolution -> EntityStack -> Result Error ( List Attribute, EntityResolution )
addAttributeListHelp parsedAttributeList reversedAttributeList entityResolution entityStack =
    case parsedAttributeList of
        [] ->
            Ok ( reversedAttributeList, entityResolution )

        first :: rest ->
            let
                addResult =
                    addAttribute first entityResolution entityStack
            in
            case addResult of
                Ok ( attribute, updatedResolution ) ->
                    addAttributeListHelp rest
                        (attribute :: reversedAttributeList)
                        updatedResolution
                        entityStack

                Err error ->
                    Err error


addEntities : List ( Int, ParsedEntity ) -> EntityResolution -> Result Error EntityResolution
addEntities parsedEntityInstances entityResolution =
    case parsedEntityInstances of
        [] ->
            Ok entityResolution

        ( id, parsedEntity ) :: rest ->
            let
                entityStack =
                    EntityStack.singleton id

                addResult =
                    addEntity id parsedEntity entityResolution entityStack
                        |> Result.map Tuple.second
            in
            case addResult of
                Ok updatedResolution ->
                    addEntities rest updatedResolution

                Err _ ->
                    addResult


resolve : List ( Int, ParsedEntity ) -> Result Error { allEntities : List Entity, topLevelEntities : List Entity }
resolve parsedEntityInstances =
    let
        entityResolution =
            init parsedEntityInstances
    in
    addEntities parsedEntityInstances entityResolution
        |> Result.map
            (\{ resolvedMap, referencedIds } ->
                Dict.foldr
                    (\id entity { allEntities, topLevelEntities } ->
                        { allEntities = entity :: allEntities
                        , topLevelEntities =
                            if Set.member id referencedIds then
                                topLevelEntities

                            else
                                entity :: topLevelEntities
                        }
                    )
                    { allEntities = [], topLevelEntities = [] }
                    resolvedMap
            )
