module OpenSolid.Step.Format
    exposing
        ( stringAttribute
        , binaryAttribute
        , enumAttribute
        , date
        , boolAttribute
        , intAttribute
        , floatAttribute
        , referenceTo
        , defaultAttribute
        , nullAttribute
        , listAttribute
        , typeName
        , enumName
        , typedAttribute
        , id
        , entity
        )

{-| Various string-formatting utilities, many of which wrap their result in
a type such as AttributeValue to improve type safety.
-}

import String.Extra
import Char
import Bitwise
import Date exposing (Date, Month)
import OpenSolid.Step.Types as Types


attributeValue : String -> Types.AttributeValue
attributeValue value =
    Types.AttributeValue value


stringAttribute : String -> Types.AttributeValue
stringAttribute value =
    let
        encoded =
            String.Extra.toCodePoints value
                |> List.map codePointToString
                |> String.concat
    in
        attributeValue ("'" ++ encoded ++ "'")


apostropheCodePoint : Int
apostropheCodePoint =
    Char.toCode '\''


backslashCodePoint : Int
backslashCodePoint =
    Char.toCode '\\'


codePointToString : Int -> String
codePointToString codePoint =
    if codePoint == apostropheCodePoint then
        "''"
    else if codePoint == backslashCodePoint then
        "\\"
    else if codePoint >= 0 && codePoint <= 0x1F then
        "\\X\\" ++ hexEncode 2 codePoint
    else if codePoint >= 0x20 && codePoint <= 0x7E then
        String.fromChar (Char.fromCode codePoint)
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


date : Date -> String
date value =
    String.join ""
        [ year (Date.year value)
        , "-"
        , month (Date.month value)
        , "-"
        , day (Date.day value)
        , "T"
        , hour (Date.hour value)
        , ":"
        , minute (Date.minute value)
        , ":"
        , second (Date.second value)
        ]


year : Int -> String
year value =
    toString value


month : Month -> String
month value =
    case value of
        Date.Jan ->
            "01"

        Date.Feb ->
            "02"

        Date.Mar ->
            "03"

        Date.Apr ->
            "04"

        Date.May ->
            "05"

        Date.Jun ->
            "06"

        Date.Jul ->
            "07"

        Date.Aug ->
            "08"

        Date.Sep ->
            "09"

        Date.Oct ->
            "10"

        Date.Nov ->
            "11"

        Date.Dec ->
            "12"


day : Int -> String
day value =
    String.padLeft 2 '0' (toString value)


hour : Int -> String
hour value =
    String.padLeft 2 '0' (toString value)


minute : Int -> String
minute value =
    String.padLeft 2 '0' (toString value)


second : Int -> String
second value =
    String.padLeft 2 '0' (toString value)


boolAttribute : Bool -> Types.AttributeValue
boolAttribute bool =
    if bool then
        attributeValue ".T."
    else
        attributeValue ".F."


intAttribute : Int -> Types.AttributeValue
intAttribute value =
    attributeValue (toString value)


floatAttribute : Float -> Types.AttributeValue
floatAttribute value =
    if toFloat (round value) == value then
        -- STEP requires integer-valued floats to have a trailing '.'
        attributeValue (toString value ++ ".")
    else
        attributeValue (toString value)


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
    Types.EnumName (value |> String.toUpper |> String.Extra.replace "." "")


typedAttribute : Types.TypeName -> Types.AttributeValue -> Types.AttributeValue
typedAttribute (Types.TypeName rawTypeName) (Types.AttributeValue rawAttributeValue) =
    attributeValue (rawTypeName ++ "(" ++ rawAttributeValue ++ ")")


id : Int -> String
id value =
    "#" ++ toString value


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
