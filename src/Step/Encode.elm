module Step.Encode exposing
    ( file
    , Entity, simpleEntity, complexEntity
    , Attribute
    , derived, null, bool, int, float, string, referenceTo, enum, binary, list
    , boolAs, intAs, floatAs, stringAs, enumAs, binaryAs, listAs
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

@docs Entity, simpleEntity, complexEntity


# Attributes

@docs Attribute

@docs derived, null, bool, int, float, string, referenceTo, enum, binary, list


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs boolAs, intAs, floatAs, stringAs, enumAs, binaryAs, listAs

-}

import Dict exposing (Dict)
import Step.Entities as Entities
import Step.EnumName as EnumName
import Step.Format as Format
import Step.Header exposing (Header)
import Step.TypeName as TypeName
import Step.Types as Types


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
            simpleEntity "FILE_DESCRIPTION"
                [ list string header.description
                , string header.implementationLevel
                ]

        fileNameEntity =
            simpleEntity "FILE_NAME"
                [ string header.fileName
                , string header.timeStamp
                , list string header.author
                , list string header.organization
                , string header.preprocessorVersion
                , string header.originatingSystem
                , string header.authorization
                ]

        fileSchemaEntity =
            simpleEntity "FILE_SCHEMA"
                [ list string header.schemaIdentifiers
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


{-| Construct a single simple entity with the given type and attributes. The
type name will be capitalized if necessary. An [`IfcDirection`](http://www.buildingsmart-tech.org/ifc/IFC4/final/html/schema/ifcgeometryresource/lexical/ifcdirection.htm)
representing the positive Y direction in 3D could be created using

    direction =
        Step.simpleEntity "IFCDIRECTION"
            [ Step.list Step.float [ 0, 1, 0 ]
            ]

which might get encoded as `#1=IFCDIRECTION((0.,1.,0.));`.

If a given entity is _only_ referred to by a single other entity, you can create
it directly inside the definition of the parent entity. For example, to create
entity #121 from [this AP214 example](https://github.com/stepcode/stepcode/blob/master/data/ap214e3/as1-oc-214.stp),
you could use

    Step.simpleEntity "AXIS2_PLACEMENT_3D"
        [ Step.string ""
        , Step.referenceTo <|
            Step.simpleEntity "CARTESIAN_POINT"
                [ Step.string ""
                , Step.list Step.float [ 20, 7.5, 0 ]
                ]
        , Step.referenceTo <|
            Step.simpleEntity "DIRECTION"
                [ Step.string ""
                , Step.list Step.float [ 1, 0, 0 ]
                ]
        , Step.referenceTo <|
            Step.simpleEntity "DIRECTION"
                [ Step.string ""
                , Step.list Step.float [ 0, 0, -1 ]
                ]
        ]

When actually encoded to a STEP file, this will get converted into four separate
entities, with the top-level entity referring to the other three by their
automatically-generated IDs, something like:

    #1=AXIS2_PLACEMENT_3D('',#2,#3,#4);
    #2=CARTESIAN_POINT('',(20.,7.5,0.));
    #3=DIRECTION('',(1.,0.,0.));
    #4=DIRECTION('',(0.,0.,-1.));

-}
simpleEntity : String -> List Attribute -> Entity
simpleEntity givenTypeName givenAttributes =
    Types.SimpleEntity -1 (entityRecord givenTypeName givenAttributes)


entityRecord : String -> List Attribute -> Types.EntityRecord
entityRecord givenTypeName givenAttributes =
    { typeName = TypeName.fromString givenTypeName
    , attributes = givenAttributes
    }


{-| Construct a single 'complex entity'; for example

    Step.Encode.complexEntity
        [ ( "A", [ Step.Encode.int 1 ] )
        , ( "B"
          , [ Step.Encode.int 2
            , Step.Encode.string "three"
            ]
          )
        , ( "C", [ Step.Encode.enum "FOUR" ] )
        ]

will be encoded as

    #1=(A(1)B(2,'three')C(.FOUR.));

-}
complexEntity : List ( String, List Attribute ) -> Entity
complexEntity simpleEntities =
    Types.ComplexEntity -1 <|
        List.map
            (\( givenTypeName, givenAttributes ) ->
                entityRecord givenTypeName givenAttributes
            )
            simpleEntities


{-| Construct a reference to another STEP entity (will end up being encoded
using an integer ID in the resulting STEP file, e.g. `#123`).
-}
referenceTo : Entity -> Attribute
referenceTo entity_ =
    Types.ReferenceTo entity_


{-| The special 'derived value' attribute (`*` in the resulting STEP file).
-}
derived : Attribute
derived =
    Types.DerivedAttribute


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
escaped according to the (weird, custom) method specified in the STEP standard;
for example,

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

This function will capitalize and add periods if necessary, so both `Step.enum
"steel"` and `Step.enum ".STEEL."` will be encoded as `.STEEL.`.

-}
enum : String -> Attribute
enum value =
    Types.EnumAttribute (EnumName.fromString value)


{-| Construct a binary-valued attribute. The provided string is assumed to
already be hex encoded as required by the STEP standard.
-}
binary : String -> Attribute
binary value =
    Types.BinaryAttribute value


{-| Construct an attribute which is itself a list of other attributes. You
provide a list of values and a function to convert each of those values to an
attribute (which will usually be one of the attribute construction functions in
this module!). For example, to construct an attribute which is a list of floats:

    Step.list Step.float [ 0, 1, 0 ]

To construct a list of references to various entities:

    Step.list Step.referenceTo
        [ firstEntity
        , secondEntity
        , thirdEntity
        ]

In the odd case where you already have a `List Attribute`, you can use Elm's
built-in `identity` function as the first argument:

    Step.list identity
        [ firstAttribute
        , secondAttribute
        , thirdAttribute
        ]

-}
list : (a -> Attribute) -> List a -> Attribute
list toAttribute values =
    Types.AttributeList (List.map toAttribute values)


{-| Construct a type-tagged Boolean-valued attribute.
-}
boolAs : String -> Bool -> Attribute
boolAs givenTypeName value =
    typedAttribute givenTypeName (bool value)


{-| Construct a type-tagged integer-valued attribute.
-}
intAs : String -> Int -> Attribute
intAs givenTypeName value =
    typedAttribute givenTypeName (int value)


{-| Construct a type-tagged float-valued attribute.
-}
floatAs : String -> Float -> Attribute
floatAs givenTypeName value =
    typedAttribute givenTypeName (float value)


{-| Construct a type-tagged string-valued attribute.
-}
stringAs : String -> String -> Attribute
stringAs givenTypeName value =
    typedAttribute givenTypeName (string value)


{-| Construct a type-tagged enumeration attribute.
-}
enumAs : String -> String -> Attribute
enumAs givenTypeName value =
    typedAttribute givenTypeName (enum value)


{-| Construct a type-tagged binary-valued attribute.
-}
binaryAs : String -> String -> Attribute
binaryAs givenTypeName value =
    typedAttribute givenTypeName (binary value)


{-| Construct a type-tagged list attribute.
-}
listAs : String -> (a -> Attribute) -> List a -> Attribute
listAs givenTypeName toAttribute values =
    typedAttribute givenTypeName (list toAttribute values)


typedAttribute : String -> Attribute -> Attribute
typedAttribute givenTypeName attribute =
    Types.TypedAttribute (TypeName.fromString givenTypeName) attribute
