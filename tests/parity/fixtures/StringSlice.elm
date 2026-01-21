module Main exposing (main)

main : Int
main =
    if String.slice 1 4 "hello" == "ell" then 1 else 0
