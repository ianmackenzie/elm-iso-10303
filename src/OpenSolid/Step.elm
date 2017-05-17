module OpenSolid.Step
    exposing
        ( File
        , Header
        , Entity(..)
        , Parameter(..)
        , Value(..)
        , encode
        , parse
        )

{-| Read and write STEP files in Elm.

@docs File, Header, Entity, Parameter, Value

@docs encode, parse

-}

import Date exposing (Date)
import OpenSolid.Step.Encode as Encode


{-| Complete representation of the data stored in a STEP file.
-}
type alias File =
    { header : Header
    , entities : List ( Int, Entity )
    }


{-| Represents the data stored in the header section of a STEP file.
-}
type alias Header =
    { fileDescription : List String
    , fileName : String
    , timeStamp : Date
    , author : List String
    , organization : List String
    , preprocessorVersion : String
    , originatingSystem : String
    , authorization : String
    , schemaIdentifiers : List String
    }


{-| Represents an entity in a STEP file such as a point, a curve, an assembly
or an entire building.
-}
type Entity
    = Entity String (List Parameter) -- TYPE([params])
    | ComplexEntity (List Entity) -- ([entities])


{-| Represents one parameter (field) of an entity.
-}
type Parameter
    = Parameter Value -- <value>
    | TypedParameter String Value -- TYPE(<value>)
    | DefaultValue -- *
    | NullValue -- $


{-| Represents a parameter value such as a number, a string or a reference to
another entity.
-}
type Value
    = IntValue Int -- 123
    | FloatValue Float -- 3.14
    | StringValue String -- "Some string"
    | InstanceReference Int -- #123
    | EnumerationValue String -- .STEEL.
    | BinaryValue String -- "185A40EF"
    | ListValue (List Value) -- (1,2,3)


{-| Convert a File value to a string of STEP-encoded text.
-}
encode : File -> String
encode file =
    String.join "\n"
        [ "ISO-10303-21;"
        , "HEADER;"
        , encodeHeader file.header
        , "ENDSEC;"
        , "DATA;"
        , encodeEntities file.entities
        , "ENDSEC;"
        , "END-ISO-10303-21;\n"
        ]


encodeHeader : Header -> String
encodeHeader header =
    let
        fileDescriptionValues =
            List.map StringValue header.fileDescription

        fileDescriptionEntity =
            Entity "FILE_DESCRIPTION"
                [ Parameter (ListValue fileDescriptionValues)
                , Parameter (StringValue "2;1")
                ]

        dateTimeString =
            Encode.date header.timeStamp

        authorValues =
            List.map StringValue header.author

        organizationValues =
            List.map StringValue header.organization

        fileNameEntity =
            Entity "FILE_NAME"
                [ Parameter (StringValue header.fileName)
                , Parameter (StringValue dateTimeString)
                , Parameter (ListValue authorValues)
                , Parameter (ListValue organizationValues)
                , Parameter (StringValue header.preprocessorVersion)
                , Parameter (StringValue header.originatingSystem)
                , Parameter (StringValue header.authorization)
                ]

        schemaIdentifierValues =
            List.map StringValue header.schemaIdentifiers

        fileSchemaEntity =
            Entity "FILE_SCHEMA"
                [ Parameter (ListValue schemaIdentifierValues) ]
    in
        List.map encodeEntity
            [ fileDescriptionEntity
            , fileNameEntity
            , fileSchemaEntity
            ]
            |> String.join "\n"


encodeEntities : List ( Int, Entity ) -> String
encodeEntities entities =
    String.join "\n" (List.map encodeEntityInstance entities)


encodeEntityInstance : ( Int, Entity ) -> String
encodeEntityInstance ( id, entity ) =
    "#" ++ toString id ++ "=" ++ encodeEntity entity


encodeEntity : Entity -> String
encodeEntity entity =
    case entity of
        Entity type_ parameters ->
            let
                encodedParameters =
                    List.map encodeParameter parameters
            in
                type_ ++ "(" ++ String.join "," encodedParameters ++ ")"

        ComplexEntity entities ->
            let
                encodedEntities =
                    List.map encodeEntity entities
            in
                "(" ++ String.join "," encodedEntities ++ ")"


encodeParameter : Parameter -> String
encodeParameter parameter =
    case parameter of
        Parameter value ->
            encodeValue value

        TypedParameter type_ value ->
            type_ ++ "(" ++ encodeValue value ++ ")"

        DefaultValue ->
            "*"

        NullValue ->
            "$"


encodeValue : Value -> String
encodeValue value =
    case value of
        IntValue int ->
            toString int

        FloatValue float ->
            Encode.float float

        StringValue string ->
            "'" ++ Encode.string string ++ "'"

        InstanceReference id ->
            "#" ++ toString id

        EnumerationValue string ->
            "." ++ string ++ "."

        BinaryValue hexString ->
            "\"" ++ hexString ++ "\""

        ListValue values ->
            let
                encodedValues =
                    List.map encodeValue values
            in
                "(" ++ String.join "," encodedValues ++ ")"


{-| Attempt to parse a string of STEP-encoded text into a File value.
-}
parse : String -> Result String File
parse string =
    Err "Not implemented"
