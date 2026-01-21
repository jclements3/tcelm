module Main exposing (main)

main : Int
main =
    case compare 5 10 of
        LT -> 1
        EQ -> 0
        GT -> -1
