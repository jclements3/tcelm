module Main exposing (main)

main : Int
main =
    case List.minimum [] of
        Just _ -> 1
        Nothing -> 0
