module StepFile
    exposing
        ( ParseError(..)
        , ReadError(..)
        , StepFile
        , contents
        , entities
        , header
        , parse
        , read
        )

{-| Top-level functionality for working with STEP files.

@docs StepFile


# Reading

@docs Error, read, parse


# Accessors

@docs header, entities, contents

-}

import Dict exposing (Dict)
import Parser
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


{-| Attempt to parse a STEP file and then decode the resulting data. This is a
convenience function that wraps parsing and decoding errors into the single
common `Error` type for easier handling:

    read fileDecoder fileContents =
        Parse.file fileContents
            |> Result.mapError ParseError
            |> Result.andThen
                (\file ->
                    Decode.run fileDecoder file
                        |> Result.mapError DecodeError
                )

-}
read : Decoder StepFile a -> String -> Result ReadError a
read fileDecoder fileContents =
    parse fileContents
        |> Result.mapError ParseError
        |> Result.andThen
            (\file ->
                Decode.run fileDecoder file
                    |> Result.mapError DecodeError
            )


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


parse : String -> Result ParseError StepFile
parse fileContents =
    Parser.run Parse.file fileContents
        |> Result.mapError toSyntaxError
        |> Result.andThen
            (\( header_, parsedEntityInstances ) ->
                EntityResolution.resolve parsedEntityInstances
                    |> Result.mapError extractResolutionError
                    |> Result.map
                        (\entities_ ->
                            Types.StepFile
                                { header = header_
                                , entities = entities_
                                , contents = fileContents
                                }
                        )
            )
