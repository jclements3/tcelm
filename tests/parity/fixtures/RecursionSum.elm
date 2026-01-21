module Main exposing (main)

sum : Int -> Int
sum n =
    if n <= 0 then 0 else n + sum (n - 1)

main : Int
main =
    sum 10
