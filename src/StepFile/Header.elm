module StepFile.Header
    exposing
        ( author
        , authorization
        , fileDescription
        , fileName
        , organization
        , originatingSystem
        , preprocessorVersion
        , schemaIdentifiers
        , timeStamp
        )

import Date exposing (Date)
import StepFile exposing (Header)
import StepFile.Types as Types


fileDescription : Header -> List String
fileDescription (Types.Header properties) =
    properties.fileDescription


fileName : Header -> String
fileName (Types.Header properties) =
    properties.fileName


timeStamp : Header -> Date
timeStamp (Types.Header properties) =
    properties.timeStamp


author : Header -> List String
author (Types.Header properties) =
    properties.author


organization : Header -> List String
organization (Types.Header properties) =
    properties.organization


preprocessorVersion : Header -> String
preprocessorVersion (Types.Header properties) =
    properties.preprocessorVersion


originatingSystem : Header -> String
originatingSystem (Types.Header properties) =
    properties.originatingSystem


authorization : Header -> String
authorization (Types.Header properties) =
    properties.authorization


schemaIdentifiers : Header -> List String
schemaIdentifiers (Types.Header properties) =
    properties.schemaIdentifiers
