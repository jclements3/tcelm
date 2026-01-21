module Main exposing (main)

factorial : Int -> Int
factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

main : Int
main =
    factorial 5
