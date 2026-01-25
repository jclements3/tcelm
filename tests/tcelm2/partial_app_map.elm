-- expect: 30
module Test exposing (main)

multiply : Int -> Int -> Int
multiply x y = x * y

double : Int -> Int
double = multiply 2

main =
    [1, 2, 3, 4, 5]
        |> List.map double
        |> List.sum
