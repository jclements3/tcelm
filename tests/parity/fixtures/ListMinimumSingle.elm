module Main exposing (main)

main : Int
main =
    case List.minimum [42] of
        Just x -> x
        Nothing -> 0
