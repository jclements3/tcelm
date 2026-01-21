module Main exposing (main)

isEven : Int -> Int
isEven n =
    if n == 0 then 1 else isOdd (n - 1)

isOdd : Int -> Int
isOdd n =
    if n == 0 then 0 else isEven (n - 1)

main : Int
main =
    isEven 10
