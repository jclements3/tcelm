module Main exposing (main)

main : Int
main =
    case List.head (List.reverse (List.sort [3, 1, 4, 1, 5])) of
        Just n -> n
        Nothing -> 0
