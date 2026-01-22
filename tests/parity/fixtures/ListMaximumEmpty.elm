module Main exposing (main)

main : Int
main =
    case List.maximum [] of
        Just _ -> 1
        Nothing -> 0
