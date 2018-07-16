module StepFile.Encode
    exposing
        ( binary
        , binaryAs
        , default
        , entity
        , enum
        , enumAs
        , file
        , float
        , floatAs
        , header
        , int
        , intAs
        , list
        , listAs
        , null
        , orDefault
        , orNull
        , referenceTo
        , string
        , stringAs
        )

{-| Functions for encoding data in STEP format.


# Basics

@docs file, header, entity


# Attributes

@docs referenceTo, default, null, int, float, string, enum, binary, list, orNull, orDefault


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs intAs, floatAs, stringAs, enumAs, binaryAs, listAs

-}

import Dict exposing (Dict)
import StepFile exposing (StepFile)
import StepFile.Entities as Entities
import StepFile.Entity as Entity exposing (Attribute, Entity)
import StepFile.Format as Format
import StepFile.Header as Header exposing (Header)
import StepFile.Types as Types


headerString : Header -> String
headerString (Types.Header header_) =
    let
        fileDescriptionEntity =
            entity "FILE_DESCRIPTION"
                [ list (List.map string header_.fileDescription)
                , string "2;1"
                ]

        fileNameEntity =
            entity "FILE_NAME"
                [ string header_.fileName
                , string header_.timeStamp
                , list (List.map string header_.author)
                , list (List.map string header_.organization)
                , string header_.preprocessorVersion
                , string header_.originatingSystem
                , string header_.authorization
                ]

        fileSchemaEntity =
            entity "FILE_SCHEMA"
                [ list (List.map string header_.schemaIdentifiers)
                ]

        headerEntities =
            [ fileDescriptionEntity, fileNameEntity, fileSchemaEntity ]
    in
    Entities.compile headerEntities
        |> List.map (\( id, entity_, entityString ) -> entityString)
        |> String.join "\n"


{-| Construct a complete STEP file from its header and a list of entities.
Entities will be assigned integer IDs automatically, and nested entities
(entities that reference other entities) will be 'flattened' to separate
entities referring to each other by their automatically-generated IDs.
-}
file : Header -> List Entity -> StepFile
file header_ entities =
    let
        compiledEntities =
            Entities.compile entities

        toKeyValuePair ( id, entity_, entityString ) =
            ( id, entity_ )

        indexedEntities =
            compiledEntities |> List.map toKeyValuePair |> Dict.fromList

        toEntityLine ( id, entity_, entityString ) =
            Format.id id ++ "=" ++ entityString

        entitiesString =
            compiledEntities |> List.map toEntityLine |> String.join "\n"

        contents =
            String.join "\n"
                [ "ISO-10303-21;"
                , "HEADER;"
                , headerString header_
                , "ENDSEC;"
                , "DATA;"
                , entitiesString
                , "ENDSEC;"
                , "END-ISO-10303-21;\n"
                ]
    in
    Types.StepFile
        { header = header_
        , entities = indexedEntities
        , contents = contents
        }


{-| Construct the header section of a STEP file.
-}
header :
    { fileDescription : List String
    , fileName : String
    , timeStamp : String
    , author : List String
    , organization : List String
    , preprocessorVersion : String
    , originatingSystem : String
    , authorization : String
    , schemaIdentifiers : List String
    }
    -> Header
header properties =
    Types.Header properties


{-| Construct a single STEP entity from a type name and list of attributes.
-}
entity : String -> List Attribute -> Entity
entity typeName attributes =
    Types.Entity (Format.typeName typeName) attributes


{-| Construct a reference to another STEP entity.
-}
referenceTo : Entity -> Attribute
referenceTo entity_ =
    Types.ReferenceTo entity_


{-| The special 'default value' attribute.
-}
default : Attribute
default =
    Types.DefaultAttribute


{-| The special 'null value' attribute.
-}
null : Attribute
null =
    Types.NullAttribute


{-| Construct a Boolean-valued attribute.

Boolean values are actually encoded as enumeration values `.T.` and `.F.`.

-}
bool : Bool -> Attribute
bool value =
    Types.BoolAttribute value


{-| Construct an integer-valued attribute.
-}
int : Int -> Attribute
int value =
    Types.IntAttribute value


{-| Construct a real-valued attribute.
-}
float : Float -> Attribute
float value =
    Types.FloatAttribute value


{-| Construct a string-valued attribute.
-}
string : String -> Attribute
string value =
    Types.StringAttribute value


{-| Construct an attribute that refers to an enumeration value defined in an
EXPRESS schema. Enumeration values are always encoded as all-caps with leading
and trailing periods, like `.STEEL.`; this function will capitalize and add
periods if necessary.
-}
enum : String -> Attribute
enum value =
    Types.EnumAttribute (Format.enumName value)


{-| Construct a binary-valued attribute. The provided string is assumed to
already be hex encoded as required by the STEP standard.
-}
binary : String -> Attribute
binary value =
    Types.BinaryAttribute value


{-| Construct an attribute which is itself a list of other attributes.
-}
list : List Attribute -> Attribute
list attributes =
    Types.AttributeList attributes


{-| Construct a type-tagged Boolean-valued attribute.
-}
boolAs : String -> Bool -> Attribute
boolAs typeName value =
    typedAttribute typeName (bool value)


{-| Construct a type-tagged integer-valued attribute.
-}
intAs : String -> Int -> Attribute
intAs typeName value =
    typedAttribute typeName (int value)


{-| Construct a type-tagged float-valued attribute.
-}
floatAs : String -> Float -> Attribute
floatAs typeName value =
    typedAttribute typeName (float value)


{-| Construct a type-tagged string-valued attribute.
-}
stringAs : String -> String -> Attribute
stringAs typeName value =
    typedAttribute typeName (string value)


{-| Construct a type-tagged enumeration attribute.
-}
enumAs : String -> String -> Attribute
enumAs typeName value =
    typedAttribute typeName (enum value)


{-| Construct a type-tagged binary-valued attribute.
-}
binaryAs : String -> String -> Attribute
binaryAs typeName value =
    typedAttribute typeName (binary value)


{-| Construct a type-tagged list attribute.
-}
listAs : String -> List Attribute -> Attribute
listAs typeName attributes =
    typedAttribute typeName (list attributes)


typedAttribute : String -> Attribute -> Attribute
typedAttribute typeName attribute =
    Types.TypedAttribute (Format.typeName typeName) attribute


{-| Encode the value inside the given `Maybe` with the given encoder if
possible, falling back to using `null`.

    Encode.orNull Encode.int (Just 3)
    --> Encode.int 3

    Encode.orNull Encode.int Nothing
    --> Encode.null

-}
orNull : (a -> Attribute) -> Maybe a -> Attribute
orNull valueEncoder maybe =
    Maybe.map valueEncoder maybe |> Maybe.withDefault null


{-| Encode the value inside the given `Maybe` with the given encoder if
possible, falling back to using `default`.

    Encode.orDefault Encode.int (Just 3)
    --> Encode.int 3

    Encode.orDefault Encode.int Nothing
    --> Encode.default

-}
orDefault : (a -> Attribute) -> Maybe a -> Attribute
orDefault valueEncoder maybe =
    Maybe.map valueEncoder maybe |> Maybe.withDefault default
