-- expect: 10
module Test exposing (main)

sumCombine : Int -> Int -> Int
sumCombine a b = a + b

sumEmpty : Int
sumEmpty = 0

total : Int
total = List.foldl sumCombine sumEmpty [1, 2, 3, 4]

main = total
