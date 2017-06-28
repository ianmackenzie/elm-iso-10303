module OpenSolid.Step.File
    exposing
        ( Error(..)
        , decode
        , encode
        )

{-| Functions for converting STEP files to and from strings.

@docs encode, Error, decode

-}

import OpenSolid.Step as Step exposing (Attribute, Decoder, Entity, File, Header)
import OpenSolid.Step.Decode as Decode
import OpenSolid.Step.Encode as Encode
import OpenSolid.Step.EntityMap as EntityMap exposing (EntityMap)
import OpenSolid.Step.Format as Format
import OpenSolid.Step.Parse as Parse
import OpenSolid.Step.Types as Types


{-| Errors that may be encountered when loading a STEP file.
-}
type Error
    = ParseError Parse.Error
    | DecodeError String


{-| Create a STEP-encoded string that can be written out to a file. Entities
will have integer IDs generated automatically.
-}
encode : File -> String
encode file =
    String.join "\n"
        [ "ISO-10303-21;"
        , "HEADER;"
        , headerString file.header
        , "ENDSEC;"
        , "DATA;"
        , entitiesString file.entities
        , "ENDSEC;"
        , "END-ISO-10303-21;\n"
        ]


headerString : Header -> String
headerString header =
    let
        fileDescriptionEntity =
            Encode.entity "FILE_DESCRIPTION"
                [ Encode.list (List.map Encode.string header.fileDescription)
                , Encode.string "2;1"
                ]

        fileNameEntity =
            Encode.entity "FILE_NAME"
                [ Encode.string header.fileName
                , Encode.string (Format.date header.timeStamp)
                , Encode.list (List.map Encode.string header.author)
                , Encode.list (List.map Encode.string header.organization)
                , Encode.string header.preprocessorVersion
                , Encode.string header.originatingSystem
                , Encode.string header.authorization
                ]

        fileSchemaEntity =
            Encode.entity "FILE_SCHEMA"
                [ Encode.list (List.map Encode.string header.schemaIdentifiers)
                ]

        entityMap =
            EntityMap.empty
                |> addEntity fileDescriptionEntity
                |> Tuple.second
                |> addEntity fileNameEntity
                |> Tuple.second
                |> addEntity fileSchemaEntity
                |> Tuple.second
    in
    EntityMap.toList entityMap
        |> List.map Tuple.second
        |> String.join "\n"


entitiesString : List Entity -> String
entitiesString entities =
    let
        entityMap =
            List.foldl
                (\entity accumulatedMap ->
                    addEntity entity accumulatedMap |> Tuple.second
                )
                EntityMap.empty
                entities
    in
    EntityMap.toList entityMap
        |> List.map (\( id, string ) -> Format.id id ++ "=" ++ string)
        |> String.join "\n"


addEntity : Entity -> EntityMap -> ( Int, EntityMap )
addEntity (Types.Entity typeName attributes) entityMap =
    let
        ( attributeValues, mapWithAttributes ) =
            addAttributes attributes entityMap

        entityString =
            Format.entity typeName attributeValues
    in
    EntityMap.add entityString mapWithAttributes


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
        Types.DefaultAttribute ->
            ( Format.defaultAttribute, entityMap )

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

        Types.TypedAttribute typeName attribute ->
            let
                ( attributeValue, updatedMap ) =
                    addAttribute attribute entityMap
            in
            ( Format.typedAttribute typeName attributeValue, updatedMap )

        Types.AttributeList attributes ->
            let
                ( attributeValues, mapWithAttributes ) =
                    addAttributes attributes entityMap
            in
            ( Format.listAttribute attributeValues, mapWithAttributes )


{-| Attempt to parse a STEP file and then decode the resulting data with the
given decoder.
-}
decode : Decoder File a -> String -> Result Error a
decode fileDecoder fileContents =
    Parse.file fileContents
        |> Result.mapError ParseError
        |> Result.andThen
            (\file ->
                Decode.run fileDecoder file
                    |> Result.mapError DecodeError
            )
