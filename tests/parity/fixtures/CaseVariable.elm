module Main exposing (main)

main : Int
main =
    let
        x = 5
    in
    case x of
        0 -> 0
        n -> n * 10
