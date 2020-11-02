module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Step.Decode
import Step.Encode
import Step.Types as Step
import Test exposing (Test)


stringTestFile : String
stringTestFile =
    """ISO-10303-21;
HEADER;
FILE_DESCRIPTION((''),'2;1');
FILE_NAME('name','2020-11-01T16:20:00',('Ian \\X4\\0001F600\\X0\\'),(''),'\\X2\\03B1\\X0\\','elm-iso-10303','');
FILE_SCHEMA(('none'));
ENDSEC;
DATA;
#1=SIMPLE_STRING('simple string');
#2=X_STRING('see \\X\\A7 4.1');
#3=X2_STRING('pre \\X2\\03B103B203B3\\X0\\ post');
#4=X4_STRING('pre \\X4\\0001F6000001F638\\X0\\ post');
#5=MIXED_STRING('\\X4\\0001F6000001F638\\X0\\\\X\\A7\\X2\\03B1\\X0\\12\\X4\\0001F638\\X0\\3\\X\\A7\\X4\\0001F6380001F600\\X0\\');
ENDSEC;
END-ISO-10303-21;
"""


decodeFirstAttribute : Step.Decode.AttributeListDecoder Step.Attribute
decodeFirstAttribute =
    Step.Decode.attribute 0 Step.Decode.identity


testString : String -> String -> Test
testString entityType expectedString =
    let
        attributeDecoder givenDecoder =
            Step.Decode.single <|
                Step.Decode.entity entityType <|
                    Step.Decode.attribute 0 givenDecoder
    in
    Test.describe entityType
        [ Test.test "Raw attribute" <|
            \() ->
                case Step.Decode.file (attributeDecoder Step.Decode.identity) stringTestFile of
                    Ok (Step.StringAttribute value) ->
                        value |> Expect.equal expectedString

                    Ok _ ->
                        Expect.fail "Expected a string attribute"

                    Err error ->
                        Expect.fail (Debug.toString error)
        , Test.test "Decoded string" <|
            \() ->
                case Step.Decode.file (attributeDecoder Step.Decode.string) stringTestFile of
                    Ok value ->
                        value |> Expect.equal expectedString

                    Err error ->
                        Expect.fail (Debug.toString error)
        ]


suite : Test
suite =
    Test.describe "elm-iso-10303"
        [ Test.describe "String parsing"
            [ Test.test "Header fields" <|
                \() ->
                    case Step.Decode.file Step.Decode.header stringTestFile of
                        Ok header ->
                            header
                                |> Expect.all
                                    [ .fileName >> Expect.equal "name"
                                    , .author >> Expect.equal [ "Ian 😀" ]
                                    , .preprocessorVersion >> Expect.equal "α"
                                    ]

                        Err err ->
                            Expect.fail (Debug.toString err)
            , Test.describe "Entities"
                [ testString "SIMPLE_STRING" "simple string"
                , testString "X_STRING" "see § 4.1"
                , testString "X2_STRING" "pre αβγ post"
                , testString "X4_STRING" "pre 😀😸 post"
                , testString "MIXED_STRING" "😀😸§α12😸3§😸😀"
                ]
            ]
        , Test.test "String encoding" <|
            \() ->
                let
                    encoded =
                        Step.Encode.file
                            { description = [ "" ]
                            , implementationLevel = "2;1"
                            , fileName = "name"
                            , timeStamp = "2020-11-01T16:20:00"
                            , author = [ "Ian 😀" ]
                            , organization = [ "" ]
                            , preprocessorVersion = "α"
                            , originatingSystem = "elm-iso-10303"
                            , authorization = ""
                            , schemaIdentifiers = [ "none" ]
                            }
                            [ Step.Encode.entity "SIMPLE_STRING" [ Step.Encode.string "simple string" ]
                            , Step.Encode.entity "X_STRING" [ Step.Encode.string "see § 4.1" ]
                            , Step.Encode.entity "X2_STRING" [ Step.Encode.string "pre αβγ post" ]
                            , Step.Encode.entity "X4_STRING" [ Step.Encode.string "pre 😀😸 post" ]
                            , Step.Encode.entity "MIXED_STRING" [ Step.Encode.string "😀😸§α12😸3§😸😀" ]
                            ]
                in
                encoded |> Expect.equal stringTestFile
        ]
