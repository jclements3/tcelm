module Main exposing (main)

main : Int
main =
    case List.head (List.sort [-5, -2, -10, -1]) of
        Just n -> n
        Nothing -> 0
