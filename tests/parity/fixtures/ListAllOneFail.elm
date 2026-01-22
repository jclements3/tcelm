module Main exposing (main)

main : Int
main =
    if List.all (\x -> x > 2) [1, 3, 4, 5] then 1 else 0
