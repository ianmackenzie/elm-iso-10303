module Step.Parse exposing (entity, header, whitespace)

import Char
import Parser exposing ((|.), (|=), Parser)
import Step.EntityResolution as EntityResolution
import Step.Types as Types exposing (File, Header)
import String


comment : Parser ()
comment =
    -- TODO remove final 'token' call once multiComment is fixed
    Parser.multiComment "/*" "*/" Parser.NotNestable
        |. Parser.token "*/"


whitespace : Parser ()
whitespace =
    Parser.spaces
        |. Parser.loop ()
            (\() ->
                Parser.oneOf
                    [ Parser.succeed (Parser.Loop ())
                        |. comment
                        |. Parser.spaces
                    , Parser.succeed (Parser.Done ())
                    ]
            )


list : Parser a -> Parser (List a)
list parseItem =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = whitespace
        , item = parseItem
        , trailing = Parser.Forbidden
        }


isUpper : Char -> Bool
isUpper char =
    Char.isUpper char || char == '_'


isUpperOrDigit : Char -> Bool
isUpperOrDigit char =
    isUpper char || Char.isDigit char


keyword : Parser String
keyword =
    Parser.getChompedString
        (Parser.chompIf isUpper |. Parser.chompWhile isUpperOrDigit)


typeName : Parser Types.TypeName
typeName =
    Parser.succeed Types.TypeName |= keyword


string : Parser String
string =
    Parser.getChompedString
        (Parser.token "'"
            |. Parser.loop ()
                (\() ->
                    Parser.succeed identity
                        |. Parser.chompUntil "'"
                        -- TODO remove if chompUntil is changed
                        |. Parser.token "'"
                        |= Parser.oneOf
                            [ Parser.succeed (Parser.Loop ())
                                |. Parser.token "'"
                            , Parser.succeed (Parser.Done ())
                            ]
                )
        )
        -- Strip off start/end single quotation marks
        |> Parser.map (String.slice 1 -1)


binary : Parser String
binary =
    Parser.succeed identity
        |. Parser.token "\""
        |= Parser.getChompedString (Parser.chompWhile ((/=) '"'))
        |. Parser.token "\""


enum : Parser Types.EnumName
enum =
    Parser.succeed Types.EnumName
        |. Parser.token "."
        |= keyword
        |. Parser.token "."


id : Parser Int
id =
    Parser.succeed identity
        |. Parser.token "#"
        |= Parser.int


optionalSign : Parser Int
optionalSign =
    Parser.oneOf
        [ Parser.succeed -1 |. Parser.token "-"
        , Parser.succeed 1 |. Parser.token "+"
        , Parser.succeed 1
        ]


signedInt : Int -> Int -> Types.ParsedAttribute
signedInt sign value =
    Types.ParsedIntAttribute (sign * value)


signedFloat : Int -> Float -> Types.ParsedAttribute
signedFloat sign value =
    Types.ParsedFloatAttribute (toFloat sign * value)


numericAttribute : Parser Types.ParsedAttribute
numericAttribute =
    optionalSign
        |> Parser.andThen
            (\sign ->
                Parser.number
                    { int = Just (signedInt sign)
                    , float = Just (signedFloat sign)
                    , hex = Nothing
                    , octal = Nothing
                    , binary = Nothing
                    }
            )


typedAttribute : Parser Types.ParsedAttribute
typedAttribute =
    Parser.succeed Types.ParsedTypedAttribute
        |= typeName
        |. whitespace
        |. Parser.token "("
        |. whitespace
        |= lazyAttribute
        |. whitespace
        |. Parser.token ")"


attribute : Parser Types.ParsedAttribute
attribute =
    Parser.oneOf
        [ Parser.succeed Types.ParsedDefaultAttribute |. Parser.token "*"
        , Parser.succeed Types.ParsedNullAttribute |. Parser.token "$"
        , Parser.succeed (Types.ParsedBoolAttribute True) |. Parser.token ".T."
        , Parser.succeed (Types.ParsedBoolAttribute False) |. Parser.token ".F."
        , Parser.succeed Types.ParsedEnumAttribute |= enum
        , numericAttribute
        , Parser.succeed Types.ParsedStringAttribute |= string
        , Parser.succeed Types.ParsedBinaryAttribute |= binary
        , Parser.succeed Types.ParsedReference |= id
        , Parser.succeed Types.ParsedAttributeList |= list lazyAttribute
        , typedAttribute
        ]


lazyAttribute : Parser Types.ParsedAttribute
lazyAttribute =
    Parser.lazy (\() -> attribute)


entity : Parser ( Int, Types.ParsedEntity )
entity =
    Parser.succeed
        (\id_ typeName_ attributes_ ->
            ( id_, Types.ParsedSimpleEntity typeName_ attributes_ )
        )
        |= id
        |. Parser.token "="
        |= typeName
        |= list attribute


header : Parser Header
header =
    let
        start name =
            Parser.token name |. whitespace |. Parser.token "("

        end =
            Parser.token ");"

        comma =
            Parser.token ","

        stringList =
            list string
    in
    Parser.succeed
        (\fileDescription fileName timeStamp author organization preprocessorVersion originatingSystem authorization schemaIdentifiers ->
            Types.Header
                { fileDescription = fileDescription
                , fileName = fileName
                , timeStamp = timeStamp
                , author = author
                , organization = organization
                , preprocessorVersion = preprocessorVersion
                , originatingSystem = originatingSystem
                , authorization = authorization
                , schemaIdentifiers = schemaIdentifiers
                }
        )
        |. Parser.token "HEADER;"
        |. whitespace
        |. start "FILE_DESCRIPTION"
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |. Parser.token "'2;1'"
        |. whitespace
        |. end
        |. whitespace
        |. start "FILE_NAME"
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |= stringList
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. comma
        |. whitespace
        |= string
        |. whitespace
        |. end
        |. whitespace
        |. start "FILE_SCHEMA"
        |. whitespace
        |= stringList
        |. whitespace
        |. end
        |. whitespace
        |. Parser.token "ENDSEC;"
