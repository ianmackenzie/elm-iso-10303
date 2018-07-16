module StepFile.Format
    exposing
        ( binaryAttribute
        , boolAttribute
        , defaultAttribute
        , entity
        , enumAttribute
        , enumName
        , floatAttribute
        , id
        , intAttribute
        , listAttribute
        , nullAttribute
        , referenceTo
        , stringAttribute
        , typeName
        , typedAttribute
        )

{-| Various string-formatting utilities, many of which wrap their result in
a type such as AttributeValue to improve type safety.
-}

import Bitwise
import Char
import StepFile.Types as Types


attributeValue : String -> Types.AttributeValue
attributeValue value =
    Types.AttributeValue value


stringAttribute : String -> Types.AttributeValue
stringAttribute value =
    let
        encoded =
            value
                |> String.toList
                |> List.map encodedCharacter
                |> String.concat
    in
    attributeValue ("'" ++ encoded ++ "'")


apostropheCodePoint : Int
apostropheCodePoint =
    Char.toCode '\''


backslashCodePoint : Int
backslashCodePoint =
    Char.toCode '\\'


encodedCharacter : Char -> String
encodedCharacter character =
    if character == '\'' then
        "''"
    else if character == '\\' then
        "\\"
    else
        let
            codePoint =
                Char.toCode character
        in
        if codePoint >= 0 && codePoint <= 0x1F then
            "\\X\\" ++ hexEncode 2 codePoint
        else if codePoint >= 0x20 && codePoint <= 0x7E then
            String.fromChar character
        else if codePoint >= 0x7F && codePoint <= 0xFF then
            "\\X\\" ++ hexEncode 2 codePoint
        else if codePoint >= 0x0100 && codePoint <= 0xFFFF then
            "\\X2\\" ++ hexEncode 4 codePoint ++ "\\X0\\"
        else if codePoint >= 0x00010000 && codePoint <= 0x0010FFFF then
            "\\X4\\" ++ hexEncode 8 codePoint ++ "\\X0\\"
        else
            ""


hexEncode : Int -> Int -> String
hexEncode size value =
    let
        characters =
            List.range 1 size
                |> List.map (\i -> hexCharacterAtIndex (size - i) value)
    in
    String.concat characters


hexCharacterAtIndex : Int -> Int -> String
hexCharacterAtIndex index value =
    let
        hexDigit =
            Bitwise.and 0x0F (Bitwise.shiftRightBy (index * 4) value)
    in
    if hexDigit >= 0 && hexDigit <= 9 then
        String.fromChar (Char.fromCode (Char.toCode '0' + hexDigit))
    else if hexDigit >= 0 && hexDigit <= 15 then
        String.fromChar (Char.fromCode (Char.toCode 'A' + (hexDigit - 10)))
    else
        ""


binaryAttribute : String -> Types.AttributeValue
binaryAttribute value =
    attributeValue ("\"" ++ value ++ "\"")


enumAttribute : Types.EnumName -> Types.AttributeValue
enumAttribute (Types.EnumName rawEnumName) =
    attributeValue ("." ++ rawEnumName ++ ".")


boolAttribute : Bool -> Types.AttributeValue
boolAttribute bool =
    if bool then
        attributeValue ".T."
    else
        attributeValue ".F."


intAttribute : Int -> Types.AttributeValue
intAttribute value =
    attributeValue (String.fromInt value)


floatAttribute : Float -> Types.AttributeValue
floatAttribute value =
    let
        floatString =
            String.fromFloat value
    in
    attributeValue <|
        if String.contains "." floatString then
            floatString
        else
            -- No decimal point, so must be an integer-valued float; add a
            -- trailing decimal point as required by the STEP standard
            floatString ++ "."


referenceTo : Int -> Types.AttributeValue
referenceTo value =
    attributeValue (id value)


defaultAttribute : Types.AttributeValue
defaultAttribute =
    attributeValue "*"


nullAttribute : Types.AttributeValue
nullAttribute =
    attributeValue "$"


listAttribute : List Types.AttributeValue -> Types.AttributeValue
listAttribute attributeValues =
    let
        rawAttributeValues =
            attributeValues
                |> List.map
                    (\(Types.AttributeValue rawAttributeValue) ->
                        rawAttributeValue
                    )
    in
    attributeValue ("(" ++ String.join "," rawAttributeValues ++ ")")


typeName : String -> Types.TypeName
typeName value =
    Types.TypeName (String.toUpper value)


enumName : String -> Types.EnumName
enumName value =
    Types.EnumName (value |> String.toUpper |> String.replace "." "")


typedAttribute : Types.TypeName -> Types.AttributeValue -> Types.AttributeValue
typedAttribute (Types.TypeName rawTypeName) (Types.AttributeValue rawAttributeValue) =
    attributeValue (rawTypeName ++ "(" ++ rawAttributeValue ++ ")")


id : Int -> String
id value =
    "#" ++ String.fromInt value


entity : Types.TypeName -> List Types.AttributeValue -> String
entity (Types.TypeName rawTypeName) attributeValues =
    let
        rawAttributeValues =
            attributeValues
                |> List.map
                    (\(Types.AttributeValue rawAttributeValue) ->
                        rawAttributeValue
                    )
    in
    rawTypeName ++ "(" ++ String.join "," rawAttributeValues ++ ")"
