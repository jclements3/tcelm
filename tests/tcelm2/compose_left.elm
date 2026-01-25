-- expect: 8
module Test exposing (main)

double : Int -> Int
double x = x * 2

addOne : Int -> Int
addOne x = x + 1

main = (double << addOne) 3
