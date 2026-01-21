module Main exposing (main)

main : Int
main =
    case "hello" of
        "hi" -> 1
        "hello" -> 2
        "hey" -> 3
        _ -> 0
