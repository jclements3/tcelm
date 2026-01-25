-- expect: 1
module Test exposing (main)

main = if List.all (\x -> x > 0) [1, 2, 3, 4, 5] then 1 else 0
