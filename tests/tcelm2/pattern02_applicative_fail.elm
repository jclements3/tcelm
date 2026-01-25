-- expect: 0
module Test exposing (main)

add : Int -> Int -> Int
add x y = x + y

result : Maybe Int
result = Maybe.map2 add (Just 5) Nothing

main =
    case result of
        Just n -> n
        Nothing -> 0
