module Main exposing (main)

main : Int
main =
    case Result.map (\x -> x * 2) (Ok 21) of
        Ok n -> n
        Err _ -> 0
