module Main exposing (main)

main : Int
main =
    List.length (List.filterMap (\x -> if x > 100 then Just x else Nothing) [1, 2, 3])
