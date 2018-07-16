module StepFile
    exposing
        ( StepFile
        , contents
        , entities
        , header
        , with
        )

{-| Top-level functionality for working with STEP files.

@docs StepFile, with


# Properties

@docs header, entities, contents

-}

import Dict exposing (Dict)
import StepFile.Attribute as Attribute exposing (Attribute)
import StepFile.Entities as Entities
import StepFile.Entity as Entity exposing (Entity)
import StepFile.Format as Format
import StepFile.Header as Header exposing (Header)
import StepFile.Types as Types


{-| -}
type alias StepFile =
    Types.StepFile


headerString : Header -> String
headerString header_ =
    let
        fileDescriptionEntity =
            Entity.ofType "FILE_DESCRIPTION"
                [ Attribute.list <|
                    List.map Attribute.string (Header.fileDescription header_)
                , Attribute.string "2;1"
                ]

        fileNameEntity =
            Entity.ofType "FILE_NAME"
                [ Attribute.string (Header.fileName header_)
                , Attribute.string (Header.timeStamp header_)
                , Attribute.list <|
                    List.map Attribute.string (Header.author header_)
                , Attribute.list <|
                    List.map Attribute.string (Header.organization header_)
                , Attribute.string (Header.preprocessorVersion header_)
                , Attribute.string (Header.originatingSystem header_)
                , Attribute.string (Header.authorization header_)
                ]

        fileSchemaEntity =
            Entity.ofType "FILE_SCHEMA"
                [ Attribute.list <|
                    List.map Attribute.string (Header.schemaIdentifiers header_)
                ]

        headerEntities =
            [ fileDescriptionEntity, fileNameEntity, fileSchemaEntity ]
    in
    Entities.compile headerEntities
        |> List.map (\( id, entity, entityString ) -> entityString)
        |> String.join "\n"


{-| Construct a complete STEP file from a header and a list of entities.
Entities will be assigned integer IDs automatically, and nested entities
(entities that reference other entities) will be 'flattened' into separate
entities referring to each other by their automatically-generated IDs.
-}
with : { header : Header, entities : List Entity } -> StepFile
with given =
    let
        compiledEntities =
            Entities.compile given.entities

        toKeyValuePair ( id, entity_, entityString ) =
            ( id, entity_ )

        indexedEntities =
            compiledEntities |> List.map toKeyValuePair |> Dict.fromList

        toEntityLine ( id, entity_, entityString ) =
            Format.id id ++ "=" ++ entityString

        entitiesString =
            compiledEntities |> List.map toEntityLine |> String.join "\n"

        contents_ =
            String.join "\n"
                [ "ISO-10303-21;"
                , "HEADER;"
                , headerString given.header
                , "ENDSEC;"
                , "DATA;"
                , entitiesString
                , "ENDSEC;"
                , "END-ISO-10303-21;\n"
                ]
    in
    Types.StepFile
        { header = given.header
        , entities = indexedEntities
        , contents = contents_
        }


{-| Get the header of a file.
-}
header : StepFile -> Header
header (Types.StepFile stepFile) =
    stepFile.header


{-| Get a dictionary of all entities in a file, indexed by their integer ID.
-}
entities : StepFile -> Dict Int Entity
entities (Types.StepFile stepFile) =
    stepFile.entities


{-| Get the full text contents of a file as a string. Once you have constructed
a `StepFile`, you can write this string to disk to save the file.
-}
contents : StepFile -> String
contents (Types.StepFile stepFile) =
    stepFile.contents



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
