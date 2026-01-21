module Main exposing (main)

main : Int
main =
    case compare 10 5 of
        LT -> 1
        EQ -> 2
        GT -> 3
