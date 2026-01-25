-- expect: 30
module Test exposing (main)

safeDiv : Int -> Int -> Maybe Int
safeDiv a b = if b == 0 then Nothing else Just (a // b)

main =
    case do
        x <- Just 10
        y <- Just 20
        Just (x + y)
    of
        Just n -> n
        Nothing -> 0
