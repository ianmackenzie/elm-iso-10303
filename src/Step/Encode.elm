module Step.Encode exposing
    ( file
    , entity, complexEntity
    , derivedValue, null, optional, bool, int, float, string, referenceTo, enum, binary, list, tuple2, tuple3
    , boolAs, intAs, floatAs, stringAs, enumAs, binaryAs, listAs, typedAttribute
    )

{-| This module allows you to encode data in [ISO 10303-21](https://en.wikipedia.org/wiki/ISO_10303-21)
(STEP file) format.


## Example

Here is a sample STEP file (adapted from [Wikipedia](https://en.wikipedia.org/wiki/ISO_10303-21#Example)):

    ISO-10303-21;
    HEADER;
    FILE_DESCRIPTION(('A minimal AP214 example with a single part'),'2;1');
    FILE_NAME('demo','2003-12-27T11:57:53',('Lothar Klein'),('LKSoft'),' ','IDA-STEP',' ');
    FILE_SCHEMA(('AUTOMOTIVE_DESIGN { 1 0 10303 214 2 1 1}'));
    ENDSEC;
    DATA;
    #10=ORGANIZATION('O0001','LKSoft','company');
    #11=PRODUCT_DEFINITION_CONTEXT('part definition',#12,'manufacturing');
    #12=APPLICATION_CONTEXT('mechanical design');
    #13=APPLICATION_PROTOCOL_DEFINITION('','automotive_design',2003,#12);
    #14=PRODUCT_DEFINITION('0',$,#15,#11);
    #15=PRODUCT_DEFINITION_FORMATION('1',$,#16);
    #16=PRODUCT('A0001','Test Part 1','',(#18));
    #17=PRODUCT_RELATED_PRODUCT_CATEGORY('part',$,(#16));
    #18=PRODUCT_CONTEXT('',#12,'');
    #19=APPLIED_ORGANIZATION_ASSIGNMENT(#10,#20,(#16));
    #20=ORGANIZATION_ROLE('id owner');
    ENDSEC;
    END-ISO-10303-21;

To create this file using this package, you could write

    module Example exposing (..)

    import Step.Encode as Encode

    stepFile : String
    stepFile =
        let
            header =
                { description = [ "A minimal AP214 example with a single part" ]
                , implementationLevel = "2;1"
                , fileName = "demo"
                , timeStamp = "2003-12-27T11:57:53"
                , author = [ "Lothar Klein" ]
                , organization = [ "LKSoft" ]
                , preprocessorVersion = " "
                , originatingSystem = "IDA-STEP"
                , authorization = " "
                , schemaIdentifiers = [ "AUTOMOTIVE_DESIGN { 1 0 10303 214 2 1 1}" ]
                }

            applicationContext =
                Encode.entity "APPLICATION_CONTEXT"
                    [ Encode.string "mechanical design"
                    ]

            applicationProtocolDefinition =
                Encode.entity "APPLICATION_PROTOCOL_DEFINITION"
                    [ Encode.string ""
                    , Encode.string "automotive_design"
                    , Encode.int 2003
                    , Encode.referenceTo applicationContext
                    ]

            product =
                Encode.entity "PRODUCT"
                    [ Encode.string "A0001"
                    , Encode.string "Test Part 1"
                    , Encode.string ""
                    , Encode.list Encode.referenceTo
                        [ Encode.entity "PRODUCT_CONTEXT"
                            [ Encode.string ""
                            , Encode.referenceTo applicationContext
                            , Encode.string ""
                            ]
                        ]
                    ]

            productDefinition =
                Encode.entity "PRODUCT_DEFINITION"
                    [ Encode.string "0"
                    , Encode.null
                    , Encode.referenceTo <|
                        Encode.entity "PRODUCT_DEFINITION_FORMATION"
                            [ Encode.string "1"
                            , Encode.null
                            , Encode.referenceTo product
                            ]
                    , Encode.referenceTo <|
                        Encode.entity "PRODUCT_DEFINITION_CONTEXT"
                            [ Encode.string "part definition"
                            , Encode.referenceTo applicationContext
                            , Encode.string "manufacturing"
                            ]
                    ]

            productRelatedProductCategory =
                Encode.entity "PRODUCT_RELATED_PRODUCT_CATEGORY"
                    [ Encode.string "part"
                    , Encode.null
                    , Encode.list Encode.referenceTo [ product ]
                    ]

            appliedOrganizationAssignment =
                Encode.entity "APPLIED_ORGANIZATION_ASSIGNMENT"
                    [ Encode.referenceTo <|
                        Encode.entity "ORGANIZATION"
                            [ Encode.string "O0001"
                            , Encode.string "LKSoft"
                            , Encode.string "company"
                            ]
                    , Encode.referenceTo <|
                        Encode.entity "ORGANIZATION_ROLE"
                            [ Encode.string "id owner"
                            ]
                    , Encode.list Encode.referenceTo [ product ]
                    ]
        in
        Encode.file header
            [ applicationContext
            , applicationProtocolDefinition
            , productDefinition
            , productRelatedProductCategory
            , appliedOrganizationAssignment
            ]

Note that entities can be declared directly 'inside' other entities where this
makes sense, and not all entities have to be explicitly listed in the
`Encode.file` call, only top-level ones. Any entities directly or indirectly
referenced by the listed entities will also be included in the output. Entity
IDs are automatically generated and entities may be written out in arbitrary
order.

@docs file


# Entities

@docs entity, complexEntity


# Attributes

@docs derivedValue, null, optional, bool, int, float, string, referenceTo, enum, binary, list, tuple2, tuple3


## Typed attributes

Typed attributes are sometimes needed when dealing with SELECT types.

@docs boolAs, intAs, floatAs, stringAs, enumAs, binaryAs, listAs, typedAttribute

-}

import Dict exposing (Dict)
import Step.Entities as Entities
import Step.EnumValue as EnumValue
import Step.File as File exposing (Attribute, Entity, File, Header)
import Step.Format as Format
import Step.TypeName as TypeName
import Step.Types as Types


headerString : Header -> String
headerString header =
    let
        fileDescriptionEntity =
            entity "FILE_DESCRIPTION"
                [ list string header.description
                , string header.implementationLevel
                ]

        fileNameEntity =
            entity "FILE_NAME"
                [ string header.fileName
                , string header.timeStamp
                , list string header.author
                , list string header.organization
                , string header.preprocessorVersion
                , string header.originatingSystem
                , string header.authorization
                ]

        fileSchemaEntity =
            entity "FILE_SCHEMA"
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
        Encode.entity "IFCDIRECTION"
            [ Encode.list Encode.float [ 0, 1, 0 ]
            ]

which might get encoded as `#1=IFCDIRECTION((0.,1.,0.));`.

If a given entity is _only_ referred to by a single other entity, you can create
it directly inside the definition of the parent entity. For example, to create
entity #121 from [this AP214 example](https://github.com/stepcode/stepcode/blob/master/data/ap214e3/as1-oc-214.stp),
you could use

    Encode.entity "AXIS2_PLACEMENT_3D"
        [ Encode.string ""
        , Encode.referenceTo <|
            Encode.entity "CARTESIAN_POINT"
                [ Encode.string ""
                , Encode.list Encode.float [ 20, 7.5, 0 ]
                ]
        , Encode.referenceTo <|
            Encode.entity "DIRECTION"
                [ Encode.string ""
                , Encode.list Encode.float [ 1, 0, 0 ]
                ]
        , Encode.referenceTo <|
            Encode.entity "DIRECTION"
                [ Encode.string ""
                , Encode.list Encode.float [ 0, 0, -1 ]
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
entity : String -> List Attribute -> Entity
entity givenTypeName givenAttributes =
    File.SimpleEntity (TypeName.fromString givenTypeName) givenAttributes


{-| Construct a single 'complex entity'; for example

    Encode.complexEntity
        [ ( "A", [ Encode.int 1 ] )
        , ( "B"
          , [ Encode.int 2
            , Encode.string "three"
            ]
          )
        , ( "C", [ Encode.enum "FOUR" ] )
        ]

will be encoded as

    #1=(A(1)B(2,'three')C(.FOUR.));

-}
complexEntity : List ( String, List Attribute ) -> Entity
complexEntity simpleEntities =
    File.ComplexEntity (List.map (Tuple.mapFirst TypeName.fromString) simpleEntities)


{-| Construct a reference to another STEP entity (will end up being encoded
using an integer ID in the resulting STEP file, e.g. `#123`).
-}
referenceTo : Entity -> Attribute
referenceTo entity_ =
    File.ReferenceTo entity_


{-| The special 'derived value' attribute (`*` in the resulting STEP file).
-}
derivedValue : Attribute
derivedValue =
    File.DerivedValue


{-| The special 'null value' attribute (`$` in the resulting STEP file).
-}
null : Attribute
null =
    File.NullAttribute


{-| Encode a `Maybe` either using the given decoder if it is a `Just value`, or
as null if it is `Nothing`.
-}
optional : (a -> Attribute) -> Maybe a -> Attribute
optional encoder maybe =
    case maybe of
        Just value ->
            encoder value

        Nothing ->
            null


{-| Construct a Boolean-valued attribute.

Boolean values are actually encoded as enumeration values `.T.` and `.F.`.

-}
bool : Bool -> Attribute
bool value =
    File.BoolAttribute value


{-| Construct an integer-valued attribute.
-}
int : Int -> Attribute
int value =
    File.IntAttribute value


{-| Construct a real-valued attribute.
-}
float : Float -> Attribute
float value =
    File.FloatAttribute value


{-| Construct a string-valued attribute. Unicode characters will be properly
escaped according to the (weird, custom) method specified in the STEP standard;
for example,

    Encode.string "see § 4.1"

will end up being encoded as

    'see \X\A7 4.1'

-}
string : String -> Attribute
string value =
    File.StringAttribute value


{-| Construct an attribute that refers to an enumeration value defined in an
EXPRESS schema. Enumeration values are always encoded as all-caps with leading
and trailing periods, like `.STEEL.`.

This function will capitalize and add periods if necessary, so both
`Encode.enum "steel"` and `Encode.enum ".STEEL."` will be encoded as
`.STEEL.`.

-}
enum : String -> Attribute
enum value =
    File.EnumAttribute (EnumValue.fromString value)


{-| Construct a binary-valued attribute. The provided string is assumed to
already be hex encoded as required by the STEP standard.
-}
binary : String -> Attribute
binary value =
    File.BinaryAttribute value


{-| Construct an attribute which is itself a list of other attributes. You
provide a list of values and a function to convert each of those values to an
attribute (which will usually be one of the attribute construction functions in
this module!). For example, to construct an attribute which is a list of floats:

    Encode.list Encode.float [ 0, 1, 0 ]

To construct a list of references to various entities:

    Encode.list Encode.referenceTo
        [ firstEntity
        , secondEntity
        , thirdEntity
        ]

In the odd case where you already have a `List Attribute`, you can use Elm's
built-in `identity` function as the first argument:

    Encode.list identity
        [ firstAttribute
        , secondAttribute
        , thirdAttribute
        ]

-}
list : (a -> Attribute) -> List a -> Attribute
list toAttribute values =
    File.AttributeList (List.map toAttribute values)


{-| Encode a tuple of two values as a list using the given encoding function.
-}
tuple2 : (a -> Attribute) -> ( a, a ) -> Attribute
tuple2 toAttribute ( first, second ) =
    list toAttribute [ first, second ]


{-| Encode a tuple of three values as a list using the given encoding function.
-}
tuple3 : (a -> Attribute) -> ( a, a, a ) -> Attribute
tuple3 toAttribute ( first, second, third ) =
    list toAttribute [ first, second, third ]


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


{-| Construct a generic type-tagged attribute. In most cases it will be simpler
to use one of the specific functions such as `floatAs` or `enumAs`.
-}
typedAttribute : String -> Attribute -> Attribute
typedAttribute givenTypeName attribute =
    File.TypedAttribute (TypeName.fromString givenTypeName) attribute
