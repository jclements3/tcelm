module Main exposing (main)

-- Test List.map with a lambda that captures an outer variable

multiplier : Int
multiplier =
    3

main : Int
main =
    List.sum (List.map (\x -> x * multiplier) [1, 2, 3, 4])
    -- Expected: (1*3) + (2*3) + (3*3) + (4*3) = 3 + 6 + 9 + 12 = 30
