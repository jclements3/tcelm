module Main exposing (main)

main : Int
main =
    if List.all (\x -> x > 0) [] then 1 else 0
