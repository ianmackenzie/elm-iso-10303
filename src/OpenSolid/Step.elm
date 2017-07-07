module OpenSolid.Step
    exposing
        ( Attribute
        , Decoder
        , Entity
        , File
        , Header
        )

{-| The basic types used for representing and manipulating STEP files. For each
type there is an associated module that typically provides accessors and other
helper functions for working with values of that type; most of the 'real'
functionality of this package is actually in the `Encode` and `Decode` modules.

@docs File, Header, Entity, Attribute, Decoder

-}

import Date exposing (Date)
import OpenSolid.Step.Types as Types


{-| A STEP file consists of a header and a list of entities.
-}
type alias File =
    Types.File


{-| Represents the data stored in the header section of a STEP file.
-}
type alias Header =
    Types.Header


{-| Represents a single entity storied in the data section of a STEP file, such
as a point, curve, assembly or entire building.
-}
type alias Entity =
    Types.Entity


{-| Represents a single attribute of a STEP entity, such as X coordinate value,
GUID string, or a reference to another entity.
-}
type alias Attribute =
    Types.Attribute


{-| A `Decoder` describes how to attempt to decode a given `File`, `Entity` or
`Attribute` to produce a value of another type. See the `Decode` module for
details on how to use and construct decoders.
-}
type alias Decoder i a =
    Types.Decoder i a
