module Main exposing (main)

main : Int
main =
    if String.trimLeft "  hello" == "hello" then 1 else 0
