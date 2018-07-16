module StepFile.Header
    exposing
        ( Header
        , author
        , authorization
        , fileDescription
        , fileName
        , organization
        , originatingSystem
        , preprocessorVersion
        , schemaIdentifiers
        , timeStamp
        , with
        )

{-|

@docs Header, with


# Properties

@docs fileDescription, fileName, timeStamp, author, organization, preprocessorVersion, originatingSystem, authorization, schemaIdentifiers

-}

import StepFile.Types as Types


{-| Represents the data stored in the header section of a STEP file.
-}
type alias Header =
    Types.Header


{-| Construct a header from its properties. See the documentation for each
property accessor for a brief description of what each field should contain.
-}
with :
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
with properties =
    Types.Header properties


{-| Get the file description stored in a header. This is an informal description
of the contents of the file.
-}
fileDescription : Header -> List String
fileDescription (Types.Header properties) =
    properties.fileDescription


{-| Get the file name stored in a header. This may be the file name of the
actual file, or it may be an abstract name for the contents of the file used
when cross-referencing between files.
-}
fileName : Header -> String
fileName (Types.Header properties) =
    properties.fileName


{-| Get the time stamp stored in a header. This will be in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
format.
-}
timeStamp : Header -> String
timeStamp (Types.Header properties) =
    properties.timeStamp


{-| Get the author information stored in a header. This should include the name
and address of the person who created the file.
-}
author : Header -> List String
author (Types.Header properties) =
    properties.author


{-| Get the organization information stored in a header. This should be the
organization associated with the `author`.
-}
organization : Header -> List String
organization (Types.Header properties) =
    properties.organization


{-| Get the preprocessor information stored in header. This should identify
which CAD system and version created the file.
-}
preprocessorVersion : Header -> String
preprocessorVersion (Types.Header properties) =
    properties.preprocessorVersion


{-| Get the originating system information stored in a header. This may also
identify which CAD system and version created the file?
-}
originatingSystem : Header -> String
originatingSystem (Types.Header properties) =
    properties.originatingSystem


{-| Get the authorization information stored in a header. This should include
the name and address of whoever authorized sending the file.
-}
authorization : Header -> String
authorization (Types.Header properties) =
    properties.authorization


{-| Get the schema identifiers stored in a header. This will generally be a list
containing one string identifying the EXPRESS schema used by the file, which may
either be a simple string such as "IFC2X3" or an 'object identifier' such as
"AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }".
-}
schemaIdentifiers : Header -> List String
schemaIdentifiers (Types.Header properties) =
    properties.schemaIdentifiers
