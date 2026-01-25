-- expect: 8
module Test exposing (main)

add : Int -> Int -> Int
add x y = x + y

maybeX : Maybe Int
maybeX = Just 5

maybeY : Maybe Int
maybeY = Just 3

result : Maybe Int
result = Maybe.map2 add maybeX maybeY

main =
    case result of
        Just n -> n
        Nothing -> 0
