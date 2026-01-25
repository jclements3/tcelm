-- expect: 10
module Test exposing (main)

numbers : List Int
numbers = [1, 2, 3, 4]

sum : Int
sum = List.foldl (\x acc -> x + acc) 0 numbers

main = sum
