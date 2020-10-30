module Step.Entities exposing (compile)

import Dict exposing (Dict)
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.Format as Format
import Step.TypeName as TypeName exposing (TypeName)
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


formatEntityRecord : ( TypeName, List String ) -> String
formatEntityRecord ( typeName, attributeValues ) =
    TypeName.toString typeName ++ "(" ++ String.join "," attributeValues ++ ")"


addEntity : Entity -> EntityMap -> ( Int, EntityMap )
addEntity entity entityMap =
    case entity of
        Types.SimpleEntity typeName attributes ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap

                entityString =
                    formatEntityRecord ( typeName, attributeValues )
            in
            update entity entityString mapWithAttributes

        Types.ComplexEntity entityRecords ->
            let
                ( simpleEntityValues, mapWithSimpleEntities ) =
                    addEntityRecords entityRecords entityMap []

                simpleEntityStrings =
                    List.map formatEntityRecord simpleEntityValues

                entityString =
                    "(" ++ String.concat simpleEntityStrings ++ ")"
            in
            update entity entityString mapWithSimpleEntities


addEntityRecords :
    List ( TypeName, List Attribute )
    -> EntityMap
    -> List ( TypeName, List String )
    -> ( List ( TypeName, List String ), EntityMap )
addEntityRecords entityRecords entityMap accumulated =
    case entityRecords of
        ( typeName, attributes ) :: rest ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap
            in
            addEntityRecords rest mapWithAttributes <|
                (( typeName, attributeValues ) :: accumulated)

        [] ->
            ( List.reverse accumulated, entityMap )


addAttributes : List Attribute -> EntityMap -> ( List String, EntityMap )
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


addAttribute : Attribute -> EntityMap -> ( String, EntityMap )
addAttribute attribute entityMap =
    case attribute of
        Types.DerivedValue ->
            ( Format.derivedValue, entityMap )

        Types.NullAttribute ->
            ( Format.null, entityMap )

        Types.BoolAttribute bool ->
            ( Format.bool bool, entityMap )

        Types.IntAttribute int ->
            ( Format.int int, entityMap )

        Types.FloatAttribute float ->
            ( Format.float float, entityMap )

        Types.StringAttribute string ->
            ( Format.string string, entityMap )

        Types.BinaryAttribute bytes ->
            ( Format.binary bytes, entityMap )

        Types.EnumAttribute enumValue ->
            ( Format.enum enumValue, entityMap )

        Types.ReferenceTo entity ->
            let
                ( entityId, updatedMap ) =
                    addEntity entity entityMap
            in
            ( Format.id entityId, updatedMap )

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
            ( Format.list attributeValues, mapWithAttributes )


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
