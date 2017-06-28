module OpenSolid.Step.Decode.File
    exposing
        ( entities
        , entity
        , header
        )

import OpenSolid.Step exposing (Decoder, Entity, File, Header)
import OpenSolid.Step.Decode as Decode
import OpenSolid.Step.Entity as Entity
import OpenSolid.Step.Types as Types


header : Decoder File (Header -> a) -> Decoder File a
header fileDecoder =
    Types.Decoder
        (\file ->
            Decode.run fileDecoder file
                |> Result.map (\constructor -> constructor file.header)
        )


entity : String -> Decoder Entity a -> Decoder File (a -> b) -> Decoder File b
entity typeName entityDecoder fileDecoder =
    Types.Decoder
        (\file ->
            case List.filter (Entity.hasType typeName) file.entities of
                [ singleEntity ] ->
                    case Decode.run entityDecoder singleEntity of
                        Ok value ->
                            Decode.run fileDecoder file
                                |> Result.map
                                    (\constructor -> constructor value)

                        Err message ->
                            Err
                                ("In entity of type '"
                                    ++ typeName
                                    ++ "': "
                                    ++ message
                                )

                _ ->
                    Err
                        ("Expecting a single entity of type '"
                            ++ typeName
                            ++ "'"
                        )
        )


decodeAll : Decoder Entity a -> List Entity -> List a -> Result String (List a)
decodeAll entityDecoder entities accumulated =
    case entities of
        [] ->
            Ok (List.reverse accumulated)

        first :: rest ->
            case Decode.run entityDecoder first of
                Ok value ->
                    decodeAll entityDecoder rest (value :: accumulated)

                Err message ->
                    Err message


entities : String -> Decoder Entity a -> Decoder File (List a -> b) -> Decoder File b
entities typeName entityDecoder fileDecoder =
    Types.Decoder
        (\file ->
            let
                filteredEntities =
                    List.filter (Entity.hasType typeName) file.entities
            in
            case decodeAll entityDecoder filteredEntities [] of
                Ok values ->
                    Decode.run fileDecoder file
                        |> Result.map
                            (\constructor -> constructor values)

                Err message ->
                    Err
                        ("In entity of type '"
                            ++ typeName
                            ++ "': "
                            ++ message
                        )
        )
