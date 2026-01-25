-- expect: 6
module Test exposing (main)

combined : List Int
combined = [1, 2] ++ [3, 4] ++ [5, 6]

main = List.length combined
