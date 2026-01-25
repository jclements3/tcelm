-- expect: 1
module Test exposing (main)

minCombine : Int -> Int -> Int
minCombine a b =
    if a < b then a else b

numbers : List Int
numbers = [3, 1, 4, 1, 5, 9, 2, 6]

minVal : Int
minVal =
    case numbers of
        [] -> 0
        x :: xs -> List.foldl minCombine x xs

main = minVal
