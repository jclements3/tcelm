module Main exposing (main)

main : Int
main =
    case String.toInt "-42" of
        Just n -> n
        Nothing -> 0
