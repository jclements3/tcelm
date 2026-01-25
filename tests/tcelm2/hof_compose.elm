-- expect: 15
module Test exposing (main)

double : Int -> Int
double x = x * 2

addOne : Int -> Int
addOne x = x + 1

main = [1, 2, 3] |> List.map double |> List.map addOne |> List.sum
