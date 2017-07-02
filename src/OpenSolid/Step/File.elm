module OpenSolid.Step.File
    exposing
        ( Error(..)
        , contents
        , entities
        , header
        , read
        )

{-| Functions for converting STEP files to and from strings.

@docs Error, read, write

-}

import Dict exposing (Dict)
import OpenSolid.Step as Step exposing (Attribute, Decoder, Entity, File, Header)
import OpenSolid.Step.Decode as Decode
import OpenSolid.Step.Parse as Parse
import OpenSolid.Step.Types as Types


header : File -> Header
header (Types.File properties) =
    properties.header


entities : File -> Dict Int Entity
entities (Types.File properties) =
    properties.entities


contents : File -> String
contents (Types.File properties) =
    properties.contents


{-| Errors that may be encountered when loading a STEP file.
-}
type Error
    = ParseError Parse.Error
    | DecodeError String


{-| Attempt to parse a STEP file and then decode the resulting data with the
given decoder.
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
