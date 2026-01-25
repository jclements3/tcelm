-- expect: 10
module Test exposing (main)

double : Int -> Int
double x = x * 2

result : Maybe Int
result = Maybe.map double (Just 5)

main =
    case result of
        Just n -> n
        Nothing -> 0
