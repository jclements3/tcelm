module Main exposing (main)

main : Int
main =
    if List.all (\x -> x > 0) [1, 2, 3] then 1 else 0
