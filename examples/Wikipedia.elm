module Wikipedia exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import StepFile
import StepFile.Attributes as Attributes


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
            StepFile.entity "ORGANIZATION"
                [ Attributes.string "O0001"
                , Attributes.string "LKSoft"
                , Attributes.string "company"
                ]

        productDefinitionContext =
            StepFile.entity "PRODUCT_DEFINITION_CONTEXT"
                [ Attributes.string "part definition"
                , Attributes.entityReference applicationContext
                , Attributes.string "manufacturing"
                ]

        applicationContext =
            StepFile.entity "APPLICATION_CONTEXT"
                [ Attributes.string "mechanical design"
                ]

        applicationProtocolDefinition =
            StepFile.entity "APPLICATION_PROTOCOL_DEFINITION"
                [ Attributes.string ""
                , Attributes.string "automotive_design"
                , Attributes.int 2003
                , Attributes.entityReference applicationContext
                ]

        productDefinition =
            StepFile.entity "PRODUCT_DEFINITION"
                [ Attributes.string "0"
                , Attributes.null
                , Attributes.entityReference productDefinitionFormation
                , Attributes.entityReference productDefinitionContext
                ]

        productDefinitionFormation =
            StepFile.entity "PRODUCT_DEFINITION_FORMATION"
                [ Attributes.string "1"
                , Attributes.null
                , Attributes.entityReference product
                ]

        product =
            StepFile.entity "PRODUCT"
                [ Attributes.string "A0001"
                , Attributes.string "Test Part 1"
                , Attributes.string ""
                , Attributes.list [ Attributes.entityReference productContext ]
                ]

        productRelatedProductCategory =
            StepFile.entity "PRODUCT_RELATED_PRODUCT_CATEGORY"
                [ Attributes.string "part"
                , Attributes.null
                , Attributes.list [ Attributes.entityReference product ]
                ]

        productContext =
            StepFile.entity "PRODUCT_CONTEXT"
                [ Attributes.string ""
                , Attributes.entityReference applicationContext
                , Attributes.string ""
                ]

        appliedOrganizationAssignment =
            StepFile.entity "APPLIED_ORGANIZATION_ASSIGNMENT"
                [ Attributes.entityReference organization
                , Attributes.entityReference organizationRole
                , Attributes.list [ Attributes.entityReference product ]
                ]

        organizationRole =
            StepFile.entity "ORGANIZATION_ROLE"
                [ Attributes.string "id owner"
                ]
    in
    StepFile.toString header
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
