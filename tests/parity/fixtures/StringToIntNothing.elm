module Main exposing (main)

main : Int
main =
    case String.toInt "abc" of
        Just n -> n
        Nothing -> -1
