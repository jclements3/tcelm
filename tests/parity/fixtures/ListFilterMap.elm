module Main exposing (main)

main : Int
main =
    List.sum (List.filterMap (\x -> if x > 0 then Just x else Nothing) [-1, 2, -3, 4, -5])
