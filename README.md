# elm-iso-10303

This package provides functionality for encoding data in [ISO 10303-21](https://en.wikipedia.org/wiki/ISO_10303-21)
(STEP file) format. It is likely only useful as a base for higher-level packages
that support particular STEP-based file formats such as [IFC](https://en.wikipedia.org/wiki/Industry_Foundation_Classes)
and the various [STEP application protocols](https://www.steptools.com/stds/step/step_2.html).

## Example

Here is a sample STEP file (adapted from [Wikipedia](https://en.wikipedia.org/wiki/ISO_10303-21#Example)):

```
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
```

To create this file using this package, you could write

```elm
module Example exposing (..)

import Iso10303 as Step

stepFile : String
stepFile =
    let
        header =
            { fileDescription = [ "A minimal AP214 example with a single part" ]
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
            Step.entity "APPLICATION_CONTEXT"
                [ Step.string "mechanical design"
                ]

        applicationProtocolDefinition =
            Step.entity "APPLICATION_PROTOCOL_DEFINITION"
                [ Step.string ""
                , Step.string "automotive_design"
                , Step.int 2003
                , Step.referenceTo applicationContext
                ]

        product =
            Step.entity "PRODUCT"
                [ Step.string "A0001"
                , Step.string "Test Part 1"
                , Step.string ""
                , Step.list Step.referenceTo
                    [ Step.entity "PRODUCT_CONTEXT"
                        [ Step.string ""
                        , Step.referenceTo applicationContext
                        , Step.string ""
                        ]
                    ]
                ]

        productDefinition =
            Step.entity "PRODUCT_DEFINITION"
                [ Step.string "0"
                , Step.null
                , Step.referenceTo <|
                    Step.entity "PRODUCT_DEFINITION_FORMATION"
                        [ Step.string "1"
                        , Step.null
                        , Step.referenceTo product
                        ]
                , Step.referenceTo <|
                    Step.entity "PRODUCT_DEFINITION_CONTEXT"
                        [ Step.string "part definition"
                        , Step.referenceTo applicationContext
                        , Step.string "manufacturing"
                        ]
                ]

        productRelatedProductCategory =
            Step.entity "PRODUCT_RELATED_PRODUCT_CATEGORY"
                [ Step.string "part"
                , Step.null
                , Step.list Step.referenceTo [ product ]
                ]

        appliedOrganizationAssignment =
            Step.entity "APPLIED_ORGANIZATION_ASSIGNMENT"
                [ Step.referenceTo <|
                    Step.entity "ORGANIZATION"
                        [ Step.string "O0001"
                        , Step.string "LKSoft"
                        , Step.string "company"
                        ]
                , Step.referenceTo <|
                    Step.entity "ORGANIZATION_ROLE"
                        [ Step.string "id owner"
                        ]
                , Step.list Step.referenceTo [ product ]
                ]
    in
    Step.file header
        [ applicationContext
        , applicationProtocolDefinition
        , productDefinition
        , productRelatedProductCategory
        , appliedOrganizationAssignment
        ]
```

Note that entities can be declared directly 'inside' other entities where this
makes sense, and not all entities have to be explicitly listed in the
`Step.file` call, only top-level ones. (Any entities directly or indirectly
referenced by the listed entities will also be included in the output). The
above code produces

```
ISO-10303-21;
HEADER;
FILE_DESCRIPTION(('A minimal AP214 example with a single part'),'2;1');
FILE_NAME('demo','2003-12-27T11:57:53',('Lothar Klein'),('LKSoft'),' ','IDA-STEP',' ');
FILE_SCHEMA(('AUTOMOTIVE_DESIGN { 1 0 10303 214 2 1 1}'));
ENDSEC;
DATA;
#1=APPLICATION_CONTEXT('mechanical design');
#2=APPLICATION_PROTOCOL_DEFINITION('','automotive_design',2003,#1);
#3=PRODUCT_CONTEXT('',#1,'');
#4=PRODUCT('A0001','Test Part 1','',(#3));
#5=PRODUCT_DEFINITION_FORMATION('1',$,#4);
#6=PRODUCT_DEFINITION_CONTEXT('part definition',#1,'manufacturing');
#7=PRODUCT_DEFINITION('0',$,#5,#6);
#8=PRODUCT_RELATED_PRODUCT_CATEGORY('part',$,(#4));
#9=ORGANIZATION('O0001','LKSoft','company');
#10=ORGANIZATION_ROLE('id owner');
#11=APPLIED_ORGANIZATION_ASSIGNMENT(#9,#10,(#4));
ENDSEC;
END-ISO-10303-21;
```

Note that the entity IDs are automatically generated and entities may be written
out in arbitrary order.


## Questions? Comments?

Please [open a new issue](https://github.com/ianmackenzie/elm-iso-10303/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using this package, feel free to send me (**@ianmackenzie**) a message on the
[Elm Slack](http://elmlang.herokuapp.com/).
