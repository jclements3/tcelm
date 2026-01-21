module Main exposing (main)

main : Int
main =
    if String.slice 0 (-1) "hello" == "hell" then 1 else 0
