-- expect: 3
module Test exposing (main)

sumWithLength : List Int -> Int
sumWithLength list =
    case list of
        [] -> 0
        (x :: xs) as all -> 
            List.length all + sumWithLength xs

main = sumWithLength [1, 2]
