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

        organization =
            Step.entity "ORGANIZATION"
                [ Step.string "O0001"
                , Step.string "LKSoft"
                , Step.string "company"
                ]

        productDefinitionContext =
            Step.entity "PRODUCT_DEFINITION_CONTEXT"
                [ Step.string "part definition"
                , Step.referenceTo applicationContext
                , Step.string "manufacturing"
                ]

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

        productDefinition =
            Step.entity "PRODUCT_DEFINITION"
                [ Step.string "0"
                , Step.null
                , Step.referenceTo productDefinitionFormation
                , Step.referenceTo productDefinitionContext
                ]

        productDefinitionFormation =
            Step.entity "PRODUCT_DEFINITION_FORMATION"
                [ Step.string "1"
                , Step.null
                , Step.referenceTo product
                ]

        product =
            Step.entity "PRODUCT"
                [ Step.string "A0001"
                , Step.string "Test Part 1"
                , Step.string ""
                , Step.list [ Step.referenceTo productContext ]
                ]

        productRelatedProductCategory =
            Step.entity "PRODUCT_RELATED_PRODUCT_CATEGORY"
                [ Step.string "part"
                , Step.null
                , Step.list [ Step.referenceTo product ]
                ]

        productContext =
            Step.entity "PRODUCT_CONTEXT"
                [ Step.string ""
                , Step.referenceTo applicationContext
                , Step.string ""
                ]

        appliedOrganizationAssignment =
            Step.entity "APPLIED_ORGANIZATION_ASSIGNMENT"
                [ Step.referenceTo organization
                , Step.referenceTo organizationRole
                , Step.list [ Step.referenceTo product ]
                ]

        organizationRole =
            Step.entity "ORGANIZATION_ROLE"
                [ Step.string "id owner"
                ]
    in
    Step.file header
        [ organization
        , productDefinitionContext
        , applicationContext
        , applicationProtocolDefinition
        , productDefinition
        , productDefinitionFormation
        , product
        , productRelatedProductCategory
        , productContext
        , appliedOrganizationAssignment
        , organizationRole
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
