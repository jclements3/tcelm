module PatternGuardsMaybe exposing (main)


-- Test pattern guards with Maybe values
extractIfPositive : Maybe Int -> String
extractIfPositive m =
    case m of
        Just x if x > 0 -> "positive: " ++ String.fromInt x
        Just x if x < 0 -> "negative: " ++ String.fromInt x
        Just x -> "zero"
        Nothing -> "nothing"


main =
    extractIfPositive (Just 5) ++ " | " ++ extractIfPositive (Just -3) ++ " | " ++ extractIfPositive (Just 0) ++ " | " ++ extractIfPositive Nothing
