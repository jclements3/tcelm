module Main exposing (main)

double : Int -> Int
double x =
    x * 2

addOne : Int -> Int
addOne x =
    x + 1

-- Using backward composition: addOne << double
-- This is equivalent to: (\x -> addOne (double x))
main : Int
main =
    (addOne << double) 5
