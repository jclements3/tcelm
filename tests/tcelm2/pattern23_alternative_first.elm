-- expect: 42
module Test exposing (main)

m1 : Maybe Int
m1 = String.toInt "42"

m2 : Maybe Int
m2 = Just 0

result : Int
result =
    case m1 of
        Just n -> n
        Nothing ->
            case m2 of
                Just n -> n
                Nothing -> 0 - 1

main = result
