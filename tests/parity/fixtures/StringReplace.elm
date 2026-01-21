module Main exposing (main)

main : Int
main =
    if String.replace "world" "Elm" "hello world" == "hello Elm" then 1 else 0
