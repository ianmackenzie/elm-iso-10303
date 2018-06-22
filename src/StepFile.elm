module StepFile
    exposing
        ( ParseError(..)
        , ReadError(..)
        , StepFile
        , contents
        , entities
        , header
        , parse
        )

{-| Top-level functionality for working with STEP files.

@docs StepFile


# Reading

@docs Error, read, parse


# Accessors

@docs header, entities, contents

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)
import StepFile.Decode as Decode exposing (Decoder)
import StepFile.Entity as Entity exposing (Entity)
import StepFile.EntityResolution as EntityResolution
import StepFile.Header as Header exposing (Header)
import StepFile.Parse as Parse
import StepFile.Types as Types


{-| A STEP file consists of a header and a list of entities.
-}
type alias StepFile =
    Types.StepFile


{-| Get the header of a file.
-}
header : StepFile -> Header
header (Types.StepFile properties) =
    properties.header


{-| Get a dictionary of all entities in a file, indexed by their integer ID.

In most cases you should not need to deal with integer IDs directly, instead
extracting entities using functions like `Decode.entitiesOfType`. But if you
need access to the low-level STEP file structure for some reason you can get it
using this function!

-}
entities : StepFile -> Dict Int Entity
entities (Types.StepFile properties) =
    properties.entities


{-| Get the full text contents of a file as a string.

Note that this works when both encoding and parsing/decoding files:

  - The `File` value returned by `Parse.file` will have as `contents` exactly
    the given string.
  - The `File` value returned by `Encode.file` will have as `contents` a
    string generated from the given header and entities. This can then be
    written to a file, stored in a database etc.

-}
contents : StepFile -> String
contents (Types.StepFile properties) =
    properties.contents


{-| Types of errors that can be encountered when parsing a file:

  - A `SyntaxError` means an error actually parsing STEP text; this means that
    either the STEP file is improperly formatted or (more likely!) it uses
    an aspect of STEP syntax that is not yet supported by this package. The
    parameter is an error string that can be used for debugging (not suitable to
    be shown to end users).
  - A `NonexistentEntity` means that the file was parsed OK, but an error
    occurred when a reference such as `#23` was found in one entity but no
    entity with that ID existed in the file. The integer parameter is the ID of
    the nonexistent entity.
  - A `CircularReference` means that the files was parsed OK, but a circular
    reference was found between entities (this is possible in STEP but not
    currently supported by this package). The parameter is the circular
    reference chain: `[34, 34]` means that entity #34 refers to itself, while
    `[34, 5, 126, 34]` means that entity #34 refers to #5, which refers to #126,
    which refers back to #34.

-}
type ParseError
    = SyntaxError String
    | NonexistentEntity Int
    | CircularReference (List Int)


{-| Errors that may be encountered when reading a STEP file.
-}
type ReadError
    = ParseError ParseError
    | DecodeError String


toSyntaxError : List Parser.DeadEnd -> ParseError
toSyntaxError deadEnds =
    SyntaxError (Parser.deadEndsToString deadEnds)


extractResolutionError : EntityResolution.Error -> ParseError
extractResolutionError resolutionError =
    case resolutionError of
        EntityResolution.NonexistentEntity id_ ->
            NonexistentEntity id_

        EntityResolution.CircularReference chain ->
            CircularReference chain


type AccumulateContext
    = InData Int
    | InString Int
    | InComment


type alias AccumulateState =
    { context : AccumulateContext
    , dataChunks : List String
    , strings : List String
    , numStrings : Int
    }


entitySeparatorRegex : Regex
entitySeparatorRegex =
    Regex.fromString "=|;" |> Maybe.withDefault Regex.never


collectPairs : List ( Int, String ) -> List String -> Result ParseError (List ( Int, String ))
collectPairs accumulated atoms =
    case atoms of
        idString :: contentsString :: rest ->
            if String.startsWith "#" idString then
                case String.toInt (String.dropLeft 1 idString) of
                    Just id ->
                        collectPairs
                            (( id, contentsString ) :: accumulated)
                            rest

                    Nothing ->
                        Err (SyntaxError ("Entity ID \"" ++ idString ++ "\" is not valid"))
            else
                Err (SyntaxError "Expecting \"#\"")

        _ ->
            Ok (List.reverse accumulated)


accumulateChunks : String -> AccumulateState -> List Regex.Match -> Result ParseError ( List ( Int, String ), Array String )
accumulateChunks dataContents state matches =
    case state.context of
        InData startIndex ->
            case matches of
                match :: rest ->
                    case match.match of
                        "'" ->
                            let
                                dataChunk =
                                    dataContents
                                        |> String.slice startIndex match.index

                                updatedState =
                                    { context = InString (match.index + 1)
                                    , dataChunks = dataChunk :: state.dataChunks
                                    , strings = state.strings
                                    , numStrings = state.numStrings
                                    }
                            in
                            accumulateChunks dataContents updatedState rest

                        "/*" ->
                            let
                                dataChunk =
                                    dataContents
                                        |> String.slice startIndex match.index

                                updatedState =
                                    { context = InComment
                                    , dataChunks = dataChunk :: state.dataChunks
                                    , strings = state.strings
                                    , numStrings = state.numStrings
                                    }
                            in
                            accumulateChunks dataContents updatedState rest

                        "ENDSEC;" ->
                            let
                                lastDataChunk =
                                    dataContents
                                        |> String.slice startIndex match.index

                                compactedData =
                                    (lastDataChunk :: state.dataChunks)
                                        |> List.reverse
                                        |> String.concat
                                        |> String.words
                                        |> String.concat

                                strings =
                                    state.strings
                                        |> List.reverse
                                        |> Array.fromList

                                entityAtoms =
                                    compactedData
                                        |> String.dropRight 1
                                        |> Regex.split entitySeparatorRegex
                            in
                            collectPairs [] entityAtoms
                                |> Result.map
                                    (\entityPairs -> ( entityPairs, strings ))

                        _ ->
                            accumulateChunks dataContents state rest

                [] ->
                    Err (SyntaxError "Expecting \"ENDSEC;\"")

        InString startIndex ->
            case matches of
                match :: rest ->
                    if match.match == "'" then
                        let
                            string =
                                dataContents
                                    |> String.slice startIndex match.index

                            reference =
                                "%" ++ String.fromInt state.numStrings

                            updatedState =
                                { context = InData (match.index + 1)
                                , dataChunks = reference :: state.dataChunks
                                , strings = string :: state.strings
                                , numStrings = state.numStrings + 1
                                }
                        in
                        accumulateChunks dataContents updatedState rest
                    else
                        accumulateChunks dataContents state rest

                [] ->
                    Err (SyntaxError "Expecting \"'\"")

        InComment ->
            case matches of
                match :: rest ->
                    if match.match == "*/" then
                        let
                            updatedState =
                                { context = InData (match.index + 2)
                                , dataChunks = state.dataChunks
                                , strings = state.strings
                                , numStrings = state.numStrings
                                }
                        in
                        accumulateChunks dataContents updatedState rest
                    else
                        accumulateChunks dataContents state rest

                [] ->
                    Err (SyntaxError "Expecting \"*/\"")


parse : String -> Result ParseError ( List ( Int, String ), Array String )
parse fileContents =
    let
        prefixParser =
            Parser.succeed Tuple.pair
                |. Parse.whitespace
                |. Parser.token "ISO-10303-21;"
                |. Parse.whitespace
                |= Parse.header
                |. Parse.whitespace
                |. Parser.token "DATA;"
                |= Parser.getOffset
    in
    case Parser.run prefixParser fileContents of
        Ok ( header_, offset ) ->
            let
                dataContents =
                    fileContents
                        |> String.slice offset (String.length fileContents)

                beginState =
                    { context = InData 0
                    , dataChunks = []
                    , strings = []
                    , numStrings = 0
                    }

                separatorRegex =
                    Regex.fromString "'|/\\*|\\*/|ENDSEC;"
                        |> Maybe.withDefault Regex.never

                matches =
                    Regex.find separatorRegex dataContents
            in
            accumulateChunks dataContents beginState matches

        Err deadEnds ->
            Err (toSyntaxError deadEnds)
