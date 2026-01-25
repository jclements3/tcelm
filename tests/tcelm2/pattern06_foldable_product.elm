-- expect: 24
module Test exposing (main)

numbers : List Int
numbers = [1, 2, 3, 4]

product : Int
product = List.foldl (\x acc -> x * acc) 1 numbers

main = product
