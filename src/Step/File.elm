module Step.File exposing
    ( File(..), Header, Entity(..), Attribute(..)
    , header, entities
    )

{-| The types in this module are shared between the [`Encode`](Step-Encode) and
[`Decode`](Step-Decode) modules.

@docs File, Header, Entity, Attribute

@docs header, entities

-}

import Step.EnumValue as EnumValue exposing (EnumValue)
import Step.TypeName as TypeName exposing (TypeName)


{-| Represents an entire STEP file composed of a header and a list of entities.
-}
type File
    = File Header (List Entity)


{-| A `Header` represents the data stored in the header section of a STEP file:

  - `description` should be an informal description of the contents of the file.
  - `implementationLevel` will typically be `"2;1"` indicating version 2 of
    ISO 10303, conformance class 1 (which in turn means that the file has a
    single data section and no anchor or reference sections, along with several
    other restrictions). For other possible values, see section 8.2.2 of ISO
    10303-21.
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
    either a simple string like `"IFC2X3"` or an 'object identifier' such as
    `"AUTOMOTIVE_DESIGN { 1 0 10303 214 1 1 1 1 }"` (more commonly known as
    AP214).

-}
type alias Header =
    { description : List String
    , implementationLevel : String
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
building. Entities may be 'simple' (having a type and a list of attributes,
which can themselves be references to other entities) or 'complex' (effectively
a list of simple entities combined together).
-}
type Entity
    = Entity TypeName (List Attribute)
    | ComplexEntity (List ( TypeName, List Attribute ))


{-| An `Attribute` represents a single attribute of an `Entity`, such as an X
coordinate value, a GUID string, or a reference to another entity.
-}
type Attribute
    = DerivedAttribute
    | NullAttribute
    | BoolAttribute Bool
    | IntAttribute Int
    | FloatAttribute Float
    | StringAttribute String
    | BinaryAttribute String
    | EnumAttribute EnumValue
    | ReferenceTo Entity
    | TypedAttribute TypeName Attribute
    | AttributeList (List Attribute)


header : File -> Header
header (File fileHeader _) =
    fileHeader


entities : File -> List Entity
entities (File _ fileEntities) =
    fileEntities
