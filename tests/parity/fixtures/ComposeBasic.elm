module Main exposing (main)

double : Int -> Int
double x = x * 2

increment : Int -> Int
increment x = x + 1

main : Int
main =
    (double >> increment) 5
