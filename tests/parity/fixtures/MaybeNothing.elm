module Main exposing (main)

main : Int
main =
    case Nothing of
        Just n -> n
        Nothing -> 99
