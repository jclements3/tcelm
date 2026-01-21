module Main exposing (main)

main : Int
main =
    { a = 1, b = 2, c = 3 }.a + { a = 1, b = 2, c = 3 }.b + { a = 1, b = 2, c = 3 }.c
