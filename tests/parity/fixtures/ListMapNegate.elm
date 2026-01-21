module Main exposing (main)

main : Int
main =
    List.sum (List.map (\x -> negate x) [1, 2, 3])
