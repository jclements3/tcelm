module Main exposing (main)

main : Int
main =
    case List.head (List.sort (List.reverse [1, 2, 3, 4, 5])) of
        Just x -> x
        Nothing -> 0
