-- expect: 6
module Test exposing (main)

numbers : List Int
numbers = [1, 2, 3]

reversed : List Int
reversed = List.foldr (\x acc -> x :: acc) [] numbers

main = List.sum reversed
