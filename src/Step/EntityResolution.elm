module Step.EntityResolution exposing (resolve)

import Dict exposing (Dict)
import Step.EntityStack as EntityStack exposing (EntityStack)
import Step.Types as Types exposing (Error(..))


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


addEntity : Int -> Types.ParsedEntity -> EntityResolution -> EntityStack -> Result Error ( Types.Entity, EntityResolution )
addEntity id parsedEntity entityResolution entityStack =
    case Dict.get id entityResolution.resolvedMap of
        Just entity ->
            Ok ( entity, entityResolution )

        Nothing ->
            case parsedEntity of
                Types.ParsedSimpleEntity typeName parsedAttributes ->
                    addAttributeList parsedAttributes entityResolution entityStack
                        |> Result.map
                            (\( attributes, resolutionWithAttributes ) ->
                                let
                                    entity =
                                        Types.Simple (Types.SimpleEntity typeName attributes)

                                    updatedResolution =
                                        store id entity resolutionWithAttributes
                                in
                                ( entity, updatedResolution )
                            )

                Types.ParsedComplexEntity parsedSimpleEntities ->
                    addSimpleEntities parsedSimpleEntities entityResolution entityStack []
                        |> Result.map
                            (\( simpleEntities, resolutionWithSimpleEntities ) ->
                                let
                                    entity =
                                        Types.Complex (Types.ComplexEntity simpleEntities)

                                    updatedResolution =
                                        store id entity resolutionWithSimpleEntities
                                in
                                ( entity, updatedResolution )
                            )


addSimpleEntities :
    List ( Types.TypeName, List Types.ParsedAttribute )
    -> EntityResolution
    -> EntityStack
    -> List Types.SimpleEntity
    -> Result Error ( List Types.SimpleEntity, EntityResolution )
addSimpleEntities parsedSimpleEntities entityResolution entityStack accumulated =
    case parsedSimpleEntities of
        ( typeName, parsedAttributes ) :: rest ->
            case addAttributeList parsedAttributes entityResolution entityStack of
                Ok ( attributes, resolutionWithAttributes ) ->
                    addSimpleEntities rest resolutionWithAttributes entityStack <|
                        (Types.SimpleEntity typeName attributes :: accumulated)

                Err error ->
                    Err error

        [] ->
            Ok ( List.reverse accumulated, entityResolution )


addAttribute : Types.ParsedAttribute -> EntityResolution -> EntityStack -> Result Error ( Types.Attribute, EntityResolution )
addAttribute parsedAttribute entityResolution entityStack =
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
                                                , updatedResolution
                                                )
                                            )

                                Err chain ->
                                    -- Referenced entity is an ancestor of (or
                                    -- equal to) the given entity
                                    Err (CircularReference chain)

                        -- Error - no entity found with the given ID
                        Nothing ->
                            Err (NonexistentEntity id)

        Types.ParsedTypedAttribute name nestedParsedAttribute ->
            addAttribute nestedParsedAttribute entityResolution entityStack
                |> Result.map
                    (\( nestedAttribute, updatedResolution ) ->
                        ( Types.TypedAttribute name nestedAttribute
                        , updatedResolution
                        )
                    )

        Types.ParsedAttributeList parsedAttributeList ->
            addAttributeList parsedAttributeList entityResolution entityStack
                |> Result.map
                    (\( attributeList, updatedResolution ) ->
                        ( Types.AttributeList attributeList
                        , updatedResolution
                        )
                    )


addAttributeList : List Types.ParsedAttribute -> EntityResolution -> EntityStack -> Result Error ( List Types.Attribute, EntityResolution )
addAttributeList parsedAttributeList entityResolution entityStack =
    addAttributeListHelp parsedAttributeList [] entityResolution entityStack
        |> Result.map (Tuple.mapFirst List.reverse)


addAttributeListHelp : List Types.ParsedAttribute -> List Types.Attribute -> EntityResolution -> EntityStack -> Result Error ( List Types.Attribute, EntityResolution )
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


addEntities : List ( Int, Types.ParsedEntity ) -> EntityResolution -> Result Error EntityResolution
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


resolve : List ( Int, Types.ParsedEntity ) -> Result Error (Dict Int Types.Entity)
resolve parsedEntityInstances =
    let
        entityResolution =
            init parsedEntityInstances
    in
    addEntities parsedEntityInstances entityResolution
        |> Result.map .resolvedMap
