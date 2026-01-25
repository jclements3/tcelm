-- expect: 15
module Test exposing (main)

sumList : List Int -> Int
sumList = List.foldl (\x acc -> x + acc) 0

main = sumList [1, 2, 3, 4, 5]
