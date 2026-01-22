module Main exposing (main)

main : Int
main =
    if List.any (\x -> x > 10) [1, 2, 3, 4, 5] then 1 else 0
