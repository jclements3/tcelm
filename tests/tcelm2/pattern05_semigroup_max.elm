-- expect: 9
module Test exposing (main)

maxCombine : Int -> Int -> Int
maxCombine a b =
    if a > b then a else b

numbers : List Int
numbers = [3, 1, 4, 1, 5, 9, 2, 6]

maxVal : Int
maxVal =
    case numbers of
        [] -> 0
        x :: xs -> List.foldl maxCombine x xs

main = maxVal
