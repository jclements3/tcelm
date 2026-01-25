-- expect: 3
module Test exposing (main)

main = List.foldr (\x acc -> x - acc) 0 [1, 2, 3, 4, 5]
