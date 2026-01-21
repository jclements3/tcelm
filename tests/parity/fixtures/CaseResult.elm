module Main exposing (main)

getValue : Result Int Int -> Int
getValue r =
    case r of
        Ok x -> x
        Err e -> e

main : Int
main =
    getValue (Ok 42)
