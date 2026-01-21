module Main exposing (main)

main : Int
main =
    if String.trim "  hello  " == "hello" then 1 else 0
