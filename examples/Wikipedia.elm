module Wikipedia exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
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


main : Program () () ()
main =
    Browser.document
        { init = always ( (), Cmd.none )
        , update = \() () -> ( (), Cmd.none )
        , view =
            always <|
                { title = "STEP example"
                , body = [ Html.textarea [ Html.Attributes.value stepFile ] [] ]
                }
        , subscriptions = always Sub.none
        }
