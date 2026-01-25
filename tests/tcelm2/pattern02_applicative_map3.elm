-- expect: 15
module Test exposing (main)

add3 : Int -> Int -> Int -> Int
add3 x y z = x + y + z

result : Maybe Int
result = Maybe.map3 add3 (Just 3) (Just 5) (Just 7)

main =
    case result of
        Just n -> n
        Nothing -> 0
