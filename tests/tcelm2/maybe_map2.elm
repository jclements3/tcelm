-- expect: 30
module Test exposing (main)

add : Int -> Int -> Int
add x y = x + y

main =
    case Maybe.map2 add (Just 10) (Just 20) of
        Just n -> n
        Nothing -> 0
