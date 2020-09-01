module Wikipedia exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Step.Encode


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
            Step.Encode.entity "APPLICATION_CONTEXT"
                [ Step.Encode.string "mechanical design"
                ]

        applicationProtocolDefinition =
            Step.Encode.entity "APPLICATION_PROTOCOL_DEFINITION"
                [ Step.Encode.string ""
                , Step.Encode.string "automotive_design"
                , Step.Encode.int 2003
                , Step.Encode.referenceTo applicationContext
                ]

        product =
            Step.Encode.entity "PRODUCT"
                [ Step.Encode.string "A0001"
                , Step.Encode.string "Test Part 1"
                , Step.Encode.string ""
                , Step.Encode.list Step.Encode.referenceTo
                    [ Step.Encode.entity "PRODUCT_CONTEXT"
                        [ Step.Encode.string ""
                        , Step.Encode.referenceTo applicationContext
                        , Step.Encode.string ""
                        ]
                    ]
                ]

        productDefinition =
            Step.Encode.entity "PRODUCT_DEFINITION"
                [ Step.Encode.string "0"
                , Step.Encode.null
                , Step.Encode.referenceTo <|
                    Step.Encode.entity "PRODUCT_DEFINITION_FORMATION"
                        [ Step.Encode.string "1"
                        , Step.Encode.null
                        , Step.Encode.referenceTo product
                        ]
                , Step.Encode.referenceTo <|
                    Step.Encode.entity "PRODUCT_DEFINITION_CONTEXT"
                        [ Step.Encode.string "part definition"
                        , Step.Encode.referenceTo applicationContext
                        , Step.Encode.string "manufacturing"
                        ]
                ]

        productRelatedProductCategory =
            Step.Encode.entity "PRODUCT_RELATED_PRODUCT_CATEGORY"
                [ Step.Encode.string "part"
                , Step.Encode.null
                , Step.Encode.list Step.Encode.referenceTo [ product ]
                ]

        appliedOrganizationAssignment =
            Step.Encode.entity "APPLIED_ORGANIZATION_ASSIGNMENT"
                [ Step.Encode.referenceTo <|
                    Step.Encode.entity "ORGANIZATION"
                        [ Step.Encode.string "O0001"
                        , Step.Encode.string "LKSoft"
                        , Step.Encode.string "company"
                        ]
                , Step.Encode.referenceTo <|
                    Step.Encode.entity "ORGANIZATION_ROLE"
                        [ Step.Encode.string "id owner"
                        ]
                , Step.Encode.list Step.Encode.referenceTo [ product ]
                ]
    in
    Step.Encode.file header
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
