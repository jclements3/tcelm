module Main exposing (main)

main : Int
main =
    case 2 of
        1 -> 10
        2 ->
            case 3 of
                3 -> 23
                _ -> 20
        _ -> 0
