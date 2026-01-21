module Main exposing (main)

main : Int
main =
    if String.trimRight "hello  " == "hello" then 1 else 0
