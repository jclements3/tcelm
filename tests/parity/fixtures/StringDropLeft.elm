module Main exposing (main)

main : Int
main =
    if String.dropLeft 2 "hello" == "llo" then 1 else 0
