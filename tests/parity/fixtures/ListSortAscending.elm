module Main exposing (main)

main : Int
main =
    case List.head (List.sort [5, 2, 8, 1, 9]) of
        Just n -> n
        Nothing -> 0
