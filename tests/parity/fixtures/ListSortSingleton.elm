module Main exposing (main)

main : Int
main =
    case List.head (List.sort [7]) of
        Just x -> x
        Nothing -> 0
