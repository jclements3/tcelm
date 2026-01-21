module Main exposing (main)

main : Int
main =
    if String.dropRight 2 "hello" == "hel" then 1 else 0
