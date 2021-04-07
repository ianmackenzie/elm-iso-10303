module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Step.Decode as Decode exposing (Decoder)
import Step.Encode
import Step.Types as Step
import Test exposing (Test)


encodingTest : String
encodingTest =
    """ISO-10303-21;
HEADER;
FILE_DESCRIPTION((''),'2;1');
FILE_NAME('name','2020-11-01T16:20:00',('Ian \\X4\\0001F600\\X0\\'),(''),'\\X2\\03B1\\X0\\','elm-iso-10303','');
FILE_SCHEMA(());
ENDSEC;
DATA;
#1=SIMPLE_STRING('simple string');
#2=X_STRING('see \\X\\A7 4.1');
#3=X2_STRING('pre \\X2\\03B103B203B3\\X0\\ post');
#4=X4_STRING('pre \\X4\\0001F6000001F638\\X0\\ post');
#5=MIXED_STRING('\\X4\\0001F6000001F638\\X0\\\\X\\A7\\X2\\03B1\\X0\\12\\X4\\0001F638\\X0\\3\\X\\A7\\X4\\0001F6380001F600\\X0\\');
#6=BINARY_DATA("004D2");
#7=CHILD(1);
#8=PARENT(#7);
ENDSEC;
END-ISO-10303-21;
"""


testFileContents : String
testFileContents =
    """ISO-10303-21;
HEADER;
FILE_DESCRIPTION((''),'2;1');
FILE_NAME('name','2020-11-01T16:20:00',('Ian \\X4\\0001F600\\X0\\'),(''),'\\X2\\03B1\\X0\\','elm-iso-10303','');
FILE_SCHEMA(());
ENDSEC;
DATA;
#1=SIMPLE_STRING('simple string');
#2=X_STRING('see \\X\\A7 4.1');
#3=X2_STRING('pre \\X2\\03B103B203B3\\X0\\ post');
#4=X4_STRING('pre \\X4\\0001F6000001F638\\X0\\ post');
#5=MIXED_STRING('\\X4\\0001F6000001F638\\X0\\\\X\\A7\\X2\\03B1\\X0\\12\\X4\\0001F638\\X0\\3\\X\\A7\\X4\\0001F6380001F600\\X0\\');
#6=BINARY_DATA("004D2");
#7=CHILD(1);
#8=PARENT(#7);
#9=(SUB1(1)SUB2('foo','bar')SUB3(1.,2.));
#10=POINT('',(1.,2.,3.));
#11=VARIOUS_ATTRIBUTES(.T., .F., .STEEL., *, $);
#12=NULL_VALUE($);
ENDSEC;
END-ISO-10303-21;
"""


testFile : Decode.File
testFile =
    case Decode.parse testFileContents of
        Ok parsed ->
            parsed

        Err message ->
            Debug.todo message


testString : String -> String -> Test
testString entityType expectedString =
    let
        entityDecoder =
            Decode.simpleEntity1 identity
                Decode.ignoreContext
                Decode.ignoreId
                entityType
                (Decode.keep Decode.string)
    in
    Test.describe entityType
        [ Test.test "Decoded string" <|
            \() ->
                case Decode.single entityDecoder testFile () of
                    Ok value ->
                        value |> Expect.equal expectedString

                    Err error ->
                        Expect.fail (Debug.toString error)
        ]


type alias ComplexEntityData =
    { count : Int
    , foo : String
    , bar : String
    , x : Float
    , y : Float
    }


type Material
    = Steel
    | Aluminum


type alias VariousAttributes =
    { first : Bool
    , second : Bool
    , third : Material
    , fourth : Int
    , fifth : Int
    }


suite : Test
suite =
    Test.describe "elm-iso-10303"
        [ Test.test "Header" <|
            \() ->
                Decode.header testFile
                    |> Expect.all
                        [ .implementationLevel >> Expect.equal "2;1"
                        , .timeStamp >> Expect.equal "2020-11-01T16:20:00"
                        , .author >> Expect.equal [ "Ian 😀" ]
                        ]
        , Test.describe "String parsing"
            [ Test.describe "Entities"
                [ testString "SIMPLE_STRING" "simple string"
                , testString "X_STRING" "see § 4.1"
                , testString "X2_STRING" "pre αβγ post"
                , testString "X4_STRING" "pre 😀😸 post"
                , testString "MIXED_STRING" "😀😸§α12😸3§😸😀"
                ]
            ]
        , Test.test "Binary decoding" <|
            \() ->
                let
                    bytesDecoder =
                        Bytes.Decode.unsignedInt16 Bytes.BE

                    entityDecoder =
                        Decode.simpleEntity1 identity
                            Decode.ignoreContext
                            Decode.ignoreId
                            "BINARY_DATA"
                            (Decode.keep (Decode.binaryData bytesDecoder))
                in
                case Decode.single entityDecoder testFile () of
                    Ok value ->
                        value |> Expect.equal 1234

                    Err err ->
                        Expect.fail (Debug.toString err)
        , Test.test "Decoding from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Decode.simpleEntity1 identity
                            Decode.ignoreContext
                            Decode.ignoreId
                            "PARENT"
                            (Decode.keep <|
                                Decode.referenceTo <|
                                    Decode.simpleEntity1 identity
                                        Decode.ignoreContext
                                        Decode.ignoreId
                                        "CHILD"
                                        (Decode.keep Decode.int)
                            )
                in
                case Decode.single nestedDecoder testFile () of
                    Ok value ->
                        value |> Expect.equal 1

                    Err err ->
                        Expect.fail (Debug.toString err)
        , Test.test "Error message from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Decode.simpleEntity1 identity
                            Decode.ignoreContext
                            Decode.ignoreId
                            "PARENT"
                            (Decode.keep <|
                                Decode.referenceTo <|
                                    Decode.simpleEntity1 identity
                                        Decode.ignoreContext
                                        Decode.ignoreId
                                        "CHILD"
                                        (Decode.keep Decode.string)
                            )
                in
                case Decode.single nestedDecoder testFile () of
                    Ok _ ->
                        Expect.fail "Expected decoding to fail"

                    Err message ->
                        message |> Expect.equal "In #8->#7: Could not parse attribute as string"
        , Test.test "Unexpected type from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Decode.simpleEntity1 identity
                            Decode.ignoreContext
                            Decode.ignoreId
                            "PARENT"
                            (Decode.keep <|
                                Decode.referenceTo <|
                                    Decode.simpleEntity1 identity
                                        Decode.ignoreContext
                                        Decode.ignoreId
                                        "GRANDCHILD"
                                        (Decode.keep Decode.string)
                            )
                in
                case Decode.single nestedDecoder testFile () of
                    Ok _ ->
                        Expect.fail "Expected decoding to fail"

                    Err message ->
                        message |> Expect.equal "In #8->#7: Entity has unexpected type"
        , Test.test "Complex entity" <|
            \() ->
                let
                    decoder : Decode.Decoder Step.Entity () ComplexEntityData
                    decoder =
                        Decode.complexEntity3 ComplexEntityData
                            Decode.ignoreContext
                            Decode.ignoreId
                            (Decode.subEntity1 "SUB1"
                                (Decode.keep Decode.int)
                            )
                            (Decode.subEntity2 "SUB2"
                                (Decode.keep Decode.string)
                                (Decode.keep Decode.string)
                            )
                            (Decode.subEntity2 "SUB3"
                                (Decode.keep Decode.float)
                                (Decode.keep Decode.float)
                            )
                in
                case Decode.single decoder testFile () of
                    Ok values ->
                        values
                            |> Expect.all
                                [ .count >> Expect.equal 1
                                , .foo >> Expect.equal "foo"
                                , .bar >> Expect.equal "bar"
                                , .x >> Expect.within (Expect.Absolute 1.0e-12) 1
                                , .y >> Expect.within (Expect.Absolute 1.0e-12) 2
                                ]

                    Err message ->
                        Expect.fail message
        , Test.test "Point with empty string" <|
            \() ->
                let
                    decoder : Decoder Step.Entity () ( Float, Float, Float )
                    decoder =
                        Decode.simpleEntity2 identity
                            Decode.ignoreContext
                            Decode.ignoreId
                            "POINT"
                            (Decode.ignore Decode.emptyString)
                            (Decode.keep (Decode.tuple3 Decode.float))
                in
                case Decode.single decoder testFile () of
                    Ok coordinates ->
                        coordinates
                            |> Expect.all
                                [ \( x, _, _ ) -> x |> Expect.within (Expect.Absolute 1.0e-12) 1
                                , \( _, y, _ ) -> y |> Expect.within (Expect.Absolute 1.0e-12) 2
                                , \( _, _, z ) -> z |> Expect.within (Expect.Absolute 1.0e-12) 3
                                ]

                    Err message ->
                        Expect.fail message
        , Test.test "Various attributes" <|
            \() ->
                let
                    decoder =
                        Decode.simpleEntity5 VariousAttributes
                            Decode.ignoreContext
                            Decode.ignoreId
                            "VARIOUS_ATTRIBUTES"
                            (Decode.keep Decode.bool)
                            (Decode.keep Decode.bool)
                            (Decode.keep (Decode.enum [ ( "STEEL", Steel ), ( "ALUMINUM", Aluminum ) ]))
                            (Decode.keep (Decode.derivedValue 1))
                            (Decode.keep (Decode.null 2))
                in
                case Decode.single decoder testFile () of
                    Ok variousAttributes ->
                        variousAttributes
                            |> Expect.all
                                [ .first >> Expect.equal True
                                , .second >> Expect.equal False
                                , .third >> Expect.equal Steel
                                , .fourth >> Expect.equal 1
                                , .fifth >> Expect.equal 2
                                ]

                    Err message ->
                        Expect.fail message
        , Test.test "Encoding" <|
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
                            , schemaIdentifiers = []
                            }
                            [ Step.Encode.entity "SIMPLE_STRING" [ Step.Encode.string "simple string" ]
                            , Step.Encode.entity "X_STRING" [ Step.Encode.string "see § 4.1" ]
                            , Step.Encode.entity "X2_STRING" [ Step.Encode.string "pre αβγ post" ]
                            , Step.Encode.entity "X4_STRING" [ Step.Encode.string "pre 😀😸 post" ]
                            , Step.Encode.entity "MIXED_STRING" [ Step.Encode.string "😀😸§α12😸3§😸😀" ]
                            , Step.Encode.entity "BINARY_DATA"
                                [ Step.Encode.binaryData
                                    (Bytes.Encode.unsignedInt16 Bytes.BE 1234)
                                ]
                            , Step.Encode.entity "PARENT" <|
                                [ Step.Encode.referenceTo <|
                                    Step.Encode.entity "CHILD" [ Step.Encode.int 1 ]
                                ]
                            ]
                in
                encoded |> Expect.equal encodingTest
        ]
