module Tests.Decode2 exposing (suite)

import Bytes
import Bytes.Decode
import Expect
import Step.Decode2
import Step.Types as Step
import Test exposing (Test)


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


testFile : Step.Decode2.File
testFile =
    case Step.Decode2.parse testFileContents of
        Ok parsed ->
            parsed

        Err message ->
            Debug.todo message


testString : String -> String -> Test
testString entityType expectedString =
    let
        entityDecoder =
            Step.Decode2.simpleEntity1 identity
                Step.Decode2.ignoreId
                entityType
                (Step.Decode2.keep Step.Decode2.string)
    in
    Test.describe entityType
        [ Test.test "Decoded string" <|
            \() ->
                case Step.Decode2.single entityDecoder testFile of
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
                Step.Decode2.header testFile
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
                        Step.Decode2.simpleEntity1 identity
                            Step.Decode2.ignoreId
                            "BINARY_DATA"
                            (Step.Decode2.keep (Step.Decode2.binaryData bytesDecoder))
                in
                case Step.Decode2.single entityDecoder testFile of
                    Ok value ->
                        value |> Expect.equal 1234

                    Err err ->
                        Expect.fail (Debug.toString err)
        , Test.test "Decoding from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Step.Decode2.simpleEntity1 identity
                            Step.Decode2.ignoreId
                            "PARENT"
                            (Step.Decode2.keep <|
                                Step.Decode2.referenceTo <|
                                    Step.Decode2.simpleEntity1 identity
                                        Step.Decode2.ignoreId
                                        "CHILD"
                                        (Step.Decode2.keep Step.Decode2.int)
                            )
                in
                case Step.Decode2.single nestedDecoder testFile of
                    Ok value ->
                        value |> Expect.equal 1

                    Err err ->
                        Expect.fail (Debug.toString err)
        , Test.test "Error message from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Step.Decode2.simpleEntity1 identity
                            Step.Decode2.ignoreId
                            "PARENT"
                            (Step.Decode2.keep <|
                                Step.Decode2.referenceTo <|
                                    Step.Decode2.simpleEntity1 identity
                                        Step.Decode2.ignoreId
                                        "CHILD"
                                        (Step.Decode2.keep Step.Decode2.string)
                            )
                in
                case Step.Decode2.single nestedDecoder testFile of
                    Ok _ ->
                        Expect.fail "Expected decoding to fail"

                    Err message ->
                        message |> Expect.equal "In #8->#7: Could not parse attribute as string"
        , Test.test "Unexpected type from nested entity" <|
            \() ->
                let
                    nestedDecoder =
                        Step.Decode2.simpleEntity1 identity
                            Step.Decode2.ignoreId
                            "PARENT"
                            (Step.Decode2.keep <|
                                Step.Decode2.referenceTo <|
                                    Step.Decode2.simpleEntity1 identity
                                        Step.Decode2.ignoreId
                                        "GRANDCHILD"
                                        (Step.Decode2.keep Step.Decode2.string)
                            )
                in
                case Step.Decode2.single nestedDecoder testFile of
                    Ok _ ->
                        Expect.fail "Expected decoding to fail"

                    Err message ->
                        message |> Expect.equal "In #8->#7: Entity has unexpected type"
        , Test.test "Complex entity" <|
            \() ->
                let
                    decoder : Step.Decode2.Decoder Step.Entity ComplexEntityData
                    decoder =
                        Step.Decode2.complexEntity3 ComplexEntityData
                            Step.Decode2.ignoreId
                            (Step.Decode2.subEntity1 "SUB1"
                                (Step.Decode2.keep Step.Decode2.int)
                            )
                            (Step.Decode2.subEntity2 "SUB2"
                                (Step.Decode2.keep Step.Decode2.string)
                                (Step.Decode2.keep Step.Decode2.string)
                            )
                            (Step.Decode2.subEntity2 "SUB3"
                                (Step.Decode2.keep Step.Decode2.float)
                                (Step.Decode2.keep Step.Decode2.float)
                            )
                in
                case Step.Decode2.single decoder testFile of
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
                    decoder =
                        Step.Decode2.simpleEntity2 identity
                            Step.Decode2.ignoreId
                            "POINT"
                            (Step.Decode2.ignore Step.Decode2.emptyString)
                            (Step.Decode2.keep (Step.Decode2.tuple3 Step.Decode2.float))
                in
                case Step.Decode2.single decoder testFile of
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
                        Step.Decode2.simpleEntity5 VariousAttributes
                            Step.Decode2.ignoreId
                            "VARIOUS_ATTRIBUTES"
                            (Step.Decode2.keep Step.Decode2.bool)
                            (Step.Decode2.keep Step.Decode2.bool)
                            (Step.Decode2.keep (Step.Decode2.enum [ ( "STEEL", Steel ), ( "ALUMINUM", Aluminum ) ]))
                            (Step.Decode2.keep (Step.Decode2.derivedValue 1))
                            (Step.Decode2.keep (Step.Decode2.null 2))
                in
                case Step.Decode2.single decoder testFile of
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
        ]



-- To test:
--   - optional tuple
--   - fuzz tests! construct randomly-shaped entities with different kinds of
--     nested attributes and entities, check they are decoded correctly
