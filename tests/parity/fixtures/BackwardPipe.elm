module Main exposing (main)

double : Int -> Int
double x =
    x * 2

addOne : Int -> Int
addOne x =
    x + 1

main : Int
main =
    addOne <| double <| 5
