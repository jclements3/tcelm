module Main exposing (main)

main : Int
main =
    case List.head [10, 20, 30, 40, 50] of
        Just n -> n
        Nothing -> 0
