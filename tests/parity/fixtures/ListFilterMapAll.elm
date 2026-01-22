module Main exposing (main)

main : Int
main =
    List.sum (List.filterMap (\x -> Just (x * 2)) [1, 2, 3])
