-- expect: 30
module Test exposing (main)
main = List.sum (List.map (\x -> x * 2) [1, 2, 3, 4, 5])
