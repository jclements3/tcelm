module Main exposing (main)

main : Int
main =
    List.length (List.filterMap (\x -> if x > 0 then Just x else Nothing) [])
