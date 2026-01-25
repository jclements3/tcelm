-- expect: 12
module Test exposing (main)

double : Int -> Int
double x = x * 2

doubled : List Int
doubled = List.map double [1, 2, 3]

main = List.sum doubled
