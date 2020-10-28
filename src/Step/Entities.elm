module Step.Entities exposing (compile)

import Dict exposing (Dict)
import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.File as File exposing (Attribute, Entity)
import Step.Format as Format
import Step.TypeName as TypeName exposing (TypeName)


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
        File.SimpleEntity typeName attributes ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap

                entityString =
                    formatEntityRecord ( typeName, attributeValues )
            in
            update entity entityString mapWithAttributes

        File.ComplexEntity entityRecords ->
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
        File.DerivedValue ->
            ( Format.derivedValue, entityMap )

        File.NullAttribute ->
            ( Format.null, entityMap )

        File.BoolAttribute bool ->
            ( Format.bool bool, entityMap )

        File.IntAttribute int ->
            ( Format.int int, entityMap )

        File.FloatAttribute float ->
            ( Format.float float, entityMap )

        File.StringAttribute string ->
            ( Format.string string, entityMap )

        File.BinaryAttribute string ->
            ( Format.binary string, entityMap )

        File.EnumAttribute enumValue ->
            ( Format.enum enumValue, entityMap )

        File.ReferenceTo entity ->
            let
                ( entityId, updatedMap ) =
                    addEntity entity entityMap
            in
            ( Format.id entityId, updatedMap )

        File.TypedAttribute typeName typedAttribute ->
            let
                ( attributeValue, updatedMap ) =
                    addAttribute typedAttribute entityMap
            in
            ( Format.typedAttribute typeName attributeValue, updatedMap )

        File.AttributeList attributes ->
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
