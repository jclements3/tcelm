module Main exposing (main)

main : Int
main =
    case compare 0 0 of
        LT -> 1
        EQ -> 2
        GT -> 3
