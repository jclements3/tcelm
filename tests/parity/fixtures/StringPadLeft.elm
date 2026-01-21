module Main exposing (main)

main : Int
main =
    if String.padLeft 5 '0' "42" == "00042" then 1 else 0
