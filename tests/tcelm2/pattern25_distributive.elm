-- expect: 41
module Test exposing (main)

addOne : Int -> Int
addOne x = x + 1

double : Int -> Int
double x = x * 2

square : Int -> Int
square x = x * x

funcs : List (Int -> Int)
funcs = [ addOne, double, square ]

results : List Int
results = List.map (\f -> f 5) funcs

main = List.sum results
