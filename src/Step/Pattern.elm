module Step.Pattern exposing
    ( Mode
    , Pattern
    , anyCharacter
    , binaryData
    , bool
    , capture
    , capturing
    , comment
    , compile
    , derivedValue
    , endOfInput
    , enum
    , float
    , int
    , list
    , nonCapturing
    , null
    , oneOf
    , oneOrMore
    , optional
    , railway
    , referencedId
    , sequence
    , startOfInput
    , string
    , token
    , tuple
    , whitespace
    , zeroOrMore
    )

import Regex exposing (Regex)


type Pattern
    = Pattern String
    | Maybe Pattern
    | OneOf (List Pattern)
    | Capture Pattern
    | Sequence (List Pattern)
    | ZeroOrMore Pattern
    | OneOrMore Pattern


type Mode
    = Capturing
    | NonCapturing


specialCharacterRegex : Regex
specialCharacterRegex =
    Regex.fromString "[\\\\^$.|?*+()[]" |> Maybe.withDefault Regex.never


escape : String -> String
escape givenString =
    givenString |> Regex.replace specialCharacterRegex (\{ match } -> "\\" ++ match)


token : String -> Pattern
token givenString =
    Pattern (escape givenString)


capture : Pattern -> Pattern
capture =
    Capture


zeroOrMore : Pattern -> Pattern
zeroOrMore =
    ZeroOrMore


oneOrMore : Pattern -> Pattern
oneOrMore =
    OneOrMore


maybe : Pattern -> Pattern
maybe =
    Maybe


oneOf : List Pattern -> Pattern
oneOf =
    OneOf


sequence : List Pattern -> Pattern
sequence =
    Sequence


railway : Pattern -> List Pattern -> Pattern
railway separator patterns =
    maybe (sequence (List.intersperse separator (List.map railwayToken patterns)))


railwayToken : Pattern -> Pattern
railwayToken pattern =
    oneOf [ pattern, zeroOrMore anyCharacter ]


anyCharacter : Pattern
anyCharacter =
    Pattern "."


bool : Pattern
bool =
    capture (Pattern "\\.[TF]\\.")


string : Pattern
string =
    capture (Pattern "'(?:''|[^'])*'")


binaryData : Pattern
binaryData =
    capture (Pattern "\"[A-Z0-9]*\"")


float : Pattern
float =
    capture (Pattern "[-+]?\\d+\\.\\d*(?:E[-+]?\\d+)?")


int : Pattern
int =
    capture (Pattern "[-+]?\\d+")


list : Pattern -> Pattern
list item =
    let
        preClosingParenthesis =
            Pattern "(?=\\))"

        repeatingPattern =
            sequence
                [ whitespace
                , item
                , whitespace
                , oneOf [ token ",", preClosingParenthesis ]
                ]
    in
    capture <|
        sequence
            [ token "("
            , oneOf [ zeroOrMore repeatingPattern, whitespace ]
            , token ")"
            ]


tuple : List Pattern -> Pattern
tuple items =
    sequence
        [ token "("
        , whitespace
        , railway (sequence [ whitespace, token ",", whitespace ]) items
        , whitespace
        , token ")"
        ]


referencedId : Pattern
referencedId =
    capture (Pattern "#\\d+")


enum : Pattern
enum =
    capture (Pattern "\\.\\w+\\.")


null : Pattern
null =
    capture (token "$")


optional : Pattern -> Pattern
optional pattern =
    oneOf [ null, pattern ]


derivedValue : Pattern
derivedValue =
    capture (token "*")


whitespace : Pattern
whitespace =
    Pattern "\\s*"


comment : Pattern
comment =
    Pattern "/\\*[\\s\\S]*?\\*/"


startOfInput : Pattern
startOfInput =
    Pattern "^"


endOfInput : Pattern
endOfInput =
    Pattern "$"


capturing : Mode
capturing =
    Capturing


nonCapturing : Mode
nonCapturing =
    NonCapturing


compile : Pattern -> Regex
compile pattern =
    Regex.fromStringWith { caseInsensitive = True, multiline = False } (toString capturing pattern)
        |> Maybe.withDefault Regex.never


toString : Mode -> Pattern -> String
toString mode pattern =
    case pattern of
        Pattern literal ->
            literal

        Maybe optionalPattern ->
            String.concat
                [ "(?:"
                , toString mode optionalPattern
                , ")?"
                ]

        OneOf patterns ->
            String.concat
                [ "(?:"
                , patterns
                    |> List.map (\alternative -> "(?:" ++ toString mode alternative ++ ")")
                    |> List.intersperse "|"
                    |> String.concat
                , ")"
                ]

        Capture capturedPattern ->
            let
                nonCapturingPattern =
                    toString nonCapturing capturedPattern
            in
            case mode of
                Capturing ->
                    "(" ++ nonCapturingPattern ++ ")"

                NonCapturing ->
                    nonCapturingPattern

        Sequence patterns ->
            String.concat (List.map (toString mode) patterns)

        ZeroOrMore repeatingPattern ->
            "(?:" ++ toString mode repeatingPattern ++ ")*"

        OneOrMore repeatingPattern ->
            "(?:" ++ toString mode repeatingPattern ++ ")+"
