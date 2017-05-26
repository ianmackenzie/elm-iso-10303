module OpenSolid.Step
    exposing
        ( Attribute
        , Entity
        , Header
        )

{-| Read and write STEP files in Elm.

@docs Header, Entity, Attribute

-}

import Date exposing (Date)
import OpenSolid.Step.Types


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


{-| Represents a single entity storied in the data section of a STEP file, such
as a point, curve, assembly or entire building.
-}
type alias Entity =
    OpenSolid.Step.Types.Entity


{-| Represents a single attribute of a STEP entity, such as X coordinate value,
GUID string, or a reference to another entity.
-}
type alias Attribute =
    OpenSolid.Step.Types.Attribute
