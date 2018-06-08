module StepFile.File
    exposing
        ( Error(..)
        , contents
        , entities
        , header
        , read
        )

{-| Functions for working with STEP files.


# Accessors

@docs header, entities, contents


# All-in-one reading

@docs Error, read

-}

import Dict exposing (Dict)
import StepFile as Step exposing (Attribute, Decoder, Entity, File, Header)
import StepFile.Decode as Decode
import StepFile.Parse as Parse
import StepFile.Types as Types


{-| Get the header of a file.
-}
header : File -> Header
header (Types.File properties) =
    properties.header


{-| Get a dictionary of all entities in a file, indexed by their integer ID.

In most cases you should not need to deal with integer IDs directly, instead
extracting entities using functions like `Decode.entitiesOfType`. But if you
need access to the low-level STEP file structure for some reason you can get it
using this function!

-}
entities : File -> Dict Int Entity
entities (Types.File properties) =
    properties.entities


{-| Get the full text contents of a file as a string.

Note that this works when both encoding and parsing/decoding files:

  - The `File` value returned by `Parse.file` will have as `contents` exactly
    the given string.
  - The `File` value returned by `Encode.file` will have as `contents` a
    string generated from the given header and entities. This can then be
    written to a file, stored in a database etc.

-}
contents : File -> String
contents (Types.File properties) =
    properties.contents


{-| Errors that may be encountered when reading a STEP file.
-}
type Error
    = ParseError Parse.Error
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
read : Decoder File a -> String -> Result Error a
read fileDecoder fileContents =
    Parse.file fileContents
        |> Result.mapError ParseError
        |> Result.andThen
            (\file ->
                Decode.run fileDecoder file
                    |> Result.mapError DecodeError
            )
