module Main exposing (main)

main : Int
main =
    case List.head (List.reverse (List.range 1 5)) of
        Just n -> n
        Nothing -> 0
