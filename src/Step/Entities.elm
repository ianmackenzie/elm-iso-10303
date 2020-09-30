module Step.Entities exposing (compile)

import Dict exposing (Dict)
import Step.Format as Format
import Step.Types as Types exposing (Attribute, Entity)


type EntityMap
    = EntityMap Int (Dict String ( Int, Entity ))


buildMap : List Entity -> EntityMap
buildMap entities =
    List.foldl
        (\entity accumulatedMap ->
            addEntity entity accumulatedMap |> Tuple.second
        )
        (EntityMap 1 Dict.empty)
        entities


addEntity : Entity -> EntityMap -> ( Int, EntityMap )
addEntity entity entityMap =
    case entity of
        Types.Simple (Types.SimpleEntity typeName attributes) ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap

                entityString =
                    Format.simpleEntity ( typeName, attributeValues )
            in
            update entity entityString mapWithAttributes

        Types.Complex (Types.ComplexEntity simpleEntities) ->
            let
                ( simpleEntityValues, mapWithSimpleEntities ) =
                    addSimpleEntities simpleEntities entityMap []

                entityString =
                    Format.complexEntity simpleEntityValues
            in
            update entity entityString mapWithSimpleEntities


addSimpleEntities :
    List Types.SimpleEntity
    -> EntityMap
    -> List ( Types.TypeName, List Types.AttributeValue )
    -> ( List ( Types.TypeName, List Types.AttributeValue ), EntityMap )
addSimpleEntities simpleEntities entityMap accumulated =
    case simpleEntities of
        (Types.SimpleEntity typeName attributes) :: rest ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap
            in
            addSimpleEntities rest mapWithAttributes <|
                (( typeName, attributeValues ) :: accumulated)

        [] ->
            ( List.reverse accumulated, entityMap )


addAttributes : List Attribute -> EntityMap -> ( List Types.AttributeValue, EntityMap )
addAttributes attributes entityMap =
    List.foldl
        (\attribute ( accumulatedAttributeValues, accumulatedMap ) ->
            let
                ( attributeValue, mapWithAttribute ) =
                    addAttribute attribute accumulatedMap
            in
            ( attributeValue :: accumulatedAttributeValues
            , mapWithAttribute
            )
        )
        ( [], entityMap )
        attributes
        |> Tuple.mapFirst List.reverse


addAttribute : Attribute -> EntityMap -> ( Types.AttributeValue, EntityMap )
addAttribute attribute entityMap =
    case attribute of
        Types.DerivedAttribute ->
            ( Format.derivedAttribute, entityMap )

        Types.NullAttribute ->
            ( Format.nullAttribute, entityMap )

        Types.BoolAttribute bool ->
            ( Format.boolAttribute bool, entityMap )

        Types.IntAttribute int ->
            ( Format.intAttribute int, entityMap )

        Types.FloatAttribute float ->
            ( Format.floatAttribute float, entityMap )

        Types.StringAttribute string ->
            ( Format.stringAttribute string, entityMap )

        Types.BinaryAttribute string ->
            ( Format.binaryAttribute string, entityMap )

        Types.EnumAttribute enumName ->
            ( Format.enumAttribute enumName, entityMap )

        Types.ReferenceTo entity ->
            let
                ( entityId, updatedMap ) =
                    addEntity entity entityMap
            in
            ( Format.referenceTo entityId, updatedMap )

        Types.TypedAttribute typeName typedAttribute ->
            let
                ( attributeValue, updatedMap ) =
                    addAttribute typedAttribute entityMap
            in
            ( Format.typedAttribute typeName attributeValue, updatedMap )

        Types.AttributeList attributes ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap
            in
            ( Format.listAttribute attributeValues, mapWithAttributes )


update : Entity -> String -> EntityMap -> ( Int, EntityMap )
update entity entityString ((EntityMap nextId idMap) as entityMap) =
    case Dict.get entityString idMap of
        Just ( id, _ ) ->
            ( id, entityMap )

        Nothing ->
            ( nextId
            , EntityMap (nextId + 1)
                (Dict.insert entityString ( nextId, entity ) idMap)
            )


compile : List Entity -> List ( Int, Entity, String )
compile entities =
    let
        (EntityMap _ idMap) =
            buildMap entities
    in
    Dict.toList idMap
        |> List.map (\( string, ( id, entity ) ) -> ( id, entity, string ))
        |> List.sortBy (\( id, entity, string ) -> id)
