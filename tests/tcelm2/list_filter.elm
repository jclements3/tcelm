-- expect: 12
module Test exposing (main)
main = List.sum (List.filter (\x -> x > 2) [1, 2, 3, 4, 5])
