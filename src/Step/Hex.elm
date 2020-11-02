module Step.Hex exposing (charToInt, intToChar)


intToChar : Int -> Char
intToChar value =
    Char.fromCode <|
        if value >= 0 && value <= 9 then
            value + 48

        else if value >= 0 && value <= 15 then
            value + 55

        else
            -- Shouldn't happen
            0


charToInt : Char -> Int
charToInt char =
    let
        charCode =
            Char.toCode char
    in
    if charCode >= 48 && charCode <= 57 then
        charCode - 48

    else if charCode >= 65 && charCode <= 70 then
        charCode - 55

    else
        -- Shouldn't happen
        0
