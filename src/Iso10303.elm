module Iso10303
    exposing
        ( Attribute
        , Entity
        , Header
        , binary
        , binaryAs
        , default
        , entity
        , enum
        , enumAs
        , file
        , float
        , floatAs
        , int
        , intAs
        , list
        , listAs
        , null
        , referenceTo
        , string
        , stringAs
        )

{-| This module allows you to encode data in [ISO 10303-21](https://en.wikipedia.org/wiki/ISO_10303-21)
(STEP file) format.

The module name was chosen to avoid naming conflicts (and to emphasize that this
is a low-level package), but in most cases I recommend importing it as

    import Iso10303 as Step

All examples below assume the module has been imported this way.

@docs file


# Header

@docs Header


# Entities

@docs Entity, entity


# Attributes

@docs Attribute

@docs default, null, int, float, string, referenceTo, enum, binary, list


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs intAs, floatAs, stringAs, enumAs, binaryAs, listAs

-}

import Dict exposing (Dict)
import StepFile.Entities as Entities
import StepFile.Format as Format
import StepFile.Types as Types


{-| A `Header` represents the data stored in the header section of a STEP file:

  - `fileDescription` should be an informal description of the contents of the
    file.
  - `fileName` may be the file name of the actual file, or it may be an abstract
    name for the contents of the file used when cross-referencing between files.
  - `timeStamp` should be an
    [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)-formatted date and time.
  - `author` should include the name and address of the person who created the
    file.
  - `organization` should be the organization that the `author` is associated
    with.
  - One of `preprocessorVersion` or `originatingSystem` should identify what CAD
    program was used to generate the file. This does not seem to be used
    terribly consistently!
  - `authorization` should include the name and address of whoever authorized
    sending the file.
  - `schemaIdentifiers` identifies the EXPRESS schema used by entities in the
    file. This will usually be a list containing a single string, which may be
    either a simple string like "IFC2X3" or an 'object identifier' such as
    "AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }" (more commonly known as
    AP214).

-}
type alias Header =
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


{-| An `Entity` represents a single entity stored in the data section of a STEP
file. An entity may be a point, a curve, a part, an assembly, or even an entire
building. Every entity has a type and a list of attributes (which can themselves
be references to other entities).
-}
type alias Entity =
    Types.Entity


{-| An `Attribute` represents a single attribute of an `Entity`, such as an X
coordinate value, a GUID string, or a reference to another entity.
-}
type alias Attribute =
    Types.Attribute


headerString : Header -> String
headerString header =
    let
        fileDescriptionEntity =
            entity "FILE_DESCRIPTION"
                [ list (List.map string header.fileDescription)
                , string "2;1"
                ]

        fileNameEntity =
            entity "FILE_NAME"
                [ string header.fileName
                , string header.timeStamp
                , list (List.map string header.author)
                , list (List.map string header.organization)
                , string header.preprocessorVersion
                , string header.originatingSystem
                , string header.authorization
                ]

        fileSchemaEntity =
            entity "FILE_SCHEMA"
                [ list (List.map string header.schemaIdentifiers)
                ]

        headerEntities =
            [ fileDescriptionEntity
            , fileNameEntity
            , fileSchemaEntity
            ]
    in
    Entities.compile headerEntities
        |> List.map (\( id, entity_, entityString ) -> entityString ++ ";")
        |> String.join "\n"


{-| Build a string representing a complete STEP file from a header and a list of
entities. Entities will be assigned integer IDs automatically, and nested
entities (entities that reference other entities) will be 'flattened' into
separate entities referring to each other by their automatically-generated IDs.

Note that it is not actually necessary to list all entities explicitly, only
top-level ones; any entities that are referenced by entities in the given list
will also get included in the output.

-}
file : Header -> List Entity -> String
file header entities =
    let
        compiledEntities =
            Entities.compile entities

        toKeyValuePair ( id, entity_, entityString ) =
            ( id, entity_ )

        indexedEntities =
            compiledEntities |> List.map toKeyValuePair |> Dict.fromList

        toEntityLine ( id, entity_, entityString ) =
            Format.id id ++ "=" ++ entityString ++ ";"

        entitiesString =
            compiledEntities |> List.map toEntityLine |> String.join "\n"
    in
    String.join "\n"
        [ "ISO-10303-21;"
        , "HEADER;"
        , headerString header
        , "ENDSEC;"
        , "DATA;"
        , entitiesString
        , "ENDSEC;"
        , "END-ISO-10303-21;\n"
        ]


{-| Construct a single entity with the given type and attributes. The type name
will be capitalized if necessary. An [`IfcDirection`](http://www.buildingsmart-tech.org/ifc/IFC4/final/html/schema/ifcgeometryresource/lexical/ifcdirection.htm)
representing the positive Y direction in 3D could be created using

    direction =
        Step.entity "IFCDIRECTION"
            [ Step.list
                [ Step.float 0
                , Step.float 1
                , Step.float 0
                ]
            ]

which would get encoded as `IFCDIRECTION((0.,1.,0.))`.

-}
entity : String -> List Attribute -> Entity
entity givenTypeName givenAttributes =
    Types.Entity (Format.typeName givenTypeName) givenAttributes


{-| Construct a reference to another STEP entity (will end up being encoded
using an integer ID in the resulting STEP file, e.g. `#123`).
-}
referenceTo : Entity -> Attribute
referenceTo entity_ =
    Types.ReferenceTo entity_


{-| The special 'default value' attribute (`*` in the resulting STEP file).
-}
default : Attribute
default =
    Types.DefaultAttribute


{-| The special 'null value' attribute (`$` in the resulting STEP file).
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


{-| Construct a string-valued attribute. Unicode characters will be properly
escaped according to the (wacky, non-standard) method specified in the STEP
standard; for example,

    Step.string "see § 4.1"

will end up being encoded as

    'see \X\A7 4.1'

-}
string : String -> Attribute
string value =
    Types.StringAttribute value


{-| Construct an attribute that refers to an enumeration value defined in an
EXPRESS schema. Enumeration values are always encoded as all-caps with leading
and trailing periods, like `.STEEL.`.

This function will capitalize and add periods if necessary; so both
<code>Step.enum&nbsp;"steel"</code> and <code>Step.enum&nbsp;".STEEL."</code>
will be encoded as `.STEEL.`.

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



--{-| Types of errors that can be encountered when parsing a file:
--
--  - A `SyntaxError` means an error actually parsing STEP text; this means that
--    either the STEP file is improperly formatted or (more likely!) it uses
--    an aspect of STEP syntax that is not yet supported by this package. The
--    parameter is an error string that can be used for debugging (not suitable to
--    be shown to end users).
--  - A `NonexistentEntity` means that the file was parsed OK, but an error
--    occurred when a reference such as `#23` was found in one entity but no
--    entity with that ID existed in the file. The integer parameter is the ID of
--    the nonexistent entity.
--  - A `CircularReference` means that the files was parsed OK, but a circular
--    reference was found between entities (this is possible in STEP but not
--    currently supported by this package). The parameter is the circular
--    reference chain: `[34, 34]` means that entity #34 refers to itself, while
--    `[34, 5, 126, 34]` means that entity #34 refers to #5, which refers to #126,
--    which refers back to #34.
--
---}
--type ParseError
--    = SyntaxError String
--    | NonexistentEntity Int
--    | CircularReference (List Int)
--{-| Errors that may be encountered when reading a STEP file.
---}
--type ReadError
--    = ParseError ParseError
--    | DecodeError String
--toSyntaxError : List Parser.DeadEnd -> ParseError
--toSyntaxError deadEnds =
--    SyntaxError (Parser.deadEndsToString deadEnds)
--extractResolutionError : EntityResolution.Error -> ParseError
--extractResolutionError resolutionError =
--    case resolutionError of
--        EntityResolution.NonexistentEntity id_ ->
--            NonexistentEntity id_
--        EntityResolution.CircularReference chain ->
--            CircularReference chain
--type AccumulateContext
--    = InData
--    | InString
--    | InComment
--entitySeparatorRegex : Regex
--entitySeparatorRegex =
--    Regex.fromString "=|;" |> Maybe.withDefault Regex.never
--collectPairs : List ( Int, String ) -> List String -> Result ParseError (List ( Int, String ))
--collectPairs accumulated atoms =
--    case atoms of
--        first :: rest ->
--            case String.split "=" first of
--                [ idString, contentsString ] ->
--                    if String.startsWith "#" idString then
--                        case String.toInt (String.dropLeft 1 idString) of
--                            Just id ->
--                                collectPairs
--                                    (( id, contentsString ) :: accumulated)
--                                    rest
--                            Nothing ->
--                                Err (SyntaxError ("Entity ID \"" ++ idString ++ "\" is not valid"))
--                    else
--                        Err (SyntaxError "Expecting \"#\"")
--                _ ->
--                    Err (SyntaxError "Expected entity of the form #123=ENTITY(...)")
--        _ ->
--            Ok (List.reverse accumulated)
--accumulateChunks : String -> AccumulateContext -> Int -> List String -> List String -> Int -> List Regex.Match -> Result ParseError ( List ( Int, String ), Array String )
--accumulateChunks dataContents context startIndex dataChunks strings numStrings matches =
--    case context of
--        InData ->
--            case matches of
--                match :: rest ->
--                    case match.match of
--                        "'" ->
--                            let
--                                dataChunk =
--                                    dataContents
--                                        |> String.slice startIndex match.index
--                            in
--                            accumulateChunks dataContents
--                                InString
--                                (match.index + 1)
--                                (dataChunk :: dataChunks)
--                                strings
--                                numStrings
--                                rest
--                        "/*" ->
--                            let
--                                dataChunk =
--                                    dataContents
--                                        |> String.slice startIndex match.index
--                            in
--                            accumulateChunks dataContents
--                                InComment
--                                match.index
--                                (dataChunk :: dataChunks)
--                                strings
--                                numStrings
--                                rest
--                        "ENDSEC;" ->
--                            let
--                                lastDataChunk =
--                                    dataContents
--                                        |> String.slice startIndex match.index
--                                compactedData =
--                                    (lastDataChunk :: dataChunks)
--                                        |> List.reverse
--                                        |> String.concat
--                                        |> String.words
--                                        |> String.concat
--                                stringArray =
--                                    strings
--                                        |> List.reverse
--                                        |> Array.fromList
--                                entityAtoms =
--                                    compactedData
--                                        |> String.dropRight 1
--                                        |> String.split ";"
--                            in
--                            collectPairs [] entityAtoms
--                                |> Result.map
--                                    (\entityPairs ->
--                                        ( entityPairs, stringArray )
--                                    )
--                        _ ->
--                            accumulateChunks dataContents
--                                context
--                                startIndex
--                                dataChunks
--                                strings
--                                numStrings
--                                rest
--                [] ->
--                    Err (SyntaxError "Expecting \"ENDSEC;\"")
--        InString ->
--            case matches of
--                match :: rest ->
--                    if match.match == "'" then
--                        let
--                            string =
--                                dataContents
--                                    |> String.slice startIndex match.index
--                            reference =
--                                "%" ++ String.fromInt numStrings
--                        in
--                        accumulateChunks dataContents
--                            InData
--                            (match.index + 1)
--                            (reference :: dataChunks)
--                            (string :: strings)
--                            (numStrings + 1)
--                            rest
--                    else
--                        accumulateChunks dataContents
--                            context
--                            startIndex
--                            dataChunks
--                            strings
--                            numStrings
--                            rest
--                [] ->
--                    Err (SyntaxError "Expecting \"'\"")
--        InComment ->
--            case matches of
--                match :: rest ->
--                    if match.match == "*/" then
--                        accumulateChunks dataContents
--                            InData
--                            (match.index + 2)
--                            dataChunks
--                            strings
--                            numStrings
--                            rest
--                    else
--                        accumulateChunks dataContents
--                            context
--                            startIndex
--                            dataChunks
--                            strings
--                            numStrings
--                            rest
--                [] ->
--                    Err (SyntaxError "Expecting \"*/\"")
--parse : String -> Result ParseError ( List ( Int, String ), Array String )
--parse fileContents =
--    let
--        prefixParser =
--            Parser.succeed Tuple.pair
--                |. Parse.whitespace
--                |. Parser.token "ISO-10303-21;"
--                |. Parse.whitespace
--                |= Parse.header
--                |. Parse.whitespace
--                |. Parser.token "DATA;"
--                |= Parser.getOffset
--    in
--    case Parser.run prefixParser fileContents of
--        Ok ( header_, offset ) ->
--            let
--                dataContents =
--                    fileContents
--                        |> String.slice offset (String.length fileContents)
--                separatorRegex =
--                    Regex.fromString "'|/\\*|\\*/|ENDSEC;"
--                        |> Maybe.withDefault Regex.never
--                matches =
--                    Regex.find separatorRegex dataContents
--            in
--            accumulateChunks dataContents InData 0 [] [] 0 matches
--        Err deadEnds ->
--            Err (toSyntaxError deadEnds)
