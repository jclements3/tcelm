module Main exposing (main)

main : Int
main =
    if String.padRight 5 '-' "hi" == "hi---" then 1 else 0
