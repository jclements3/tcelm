module PatternGuards exposing (main)


classify : Int -> String
classify x =
    case x of
        n if n > 0 -> "positive"
        n if n < 0 -> "negative"
        _ -> "zero"


testGuards : Int -> Int
testGuards x =
    case x of
        n if n > 10 -> n * 2
        n if n > 5 -> n + 10
        n -> n


main =
    classify 5 ++ " " ++ classify -3 ++ " " ++ classify 0 ++ " " ++ String.fromInt (testGuards 15) ++ " " ++ String.fromInt (testGuards 7) ++ " " ++ String.fromInt (testGuards 3)
