module Main exposing (main)

main : Int
main =
    abs (negate (min 10 (max 5 (clamp 0 100 -50))))
