-- expect: [3, 2, 1]
module Test exposing (main)

-- Sort in descending order
descending a b =
    if a > b then
        LT
    else if a < b then
        GT
    else
        EQ

main =
    List.sortWith descending [1, 3, 2]
