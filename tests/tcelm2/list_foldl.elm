-- expect: 15
module Test exposing (main)
main = List.foldl (\x acc -> x + acc) 0 [1, 2, 3, 4, 5]
