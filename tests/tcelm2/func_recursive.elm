-- expect: 120
module Test exposing (main)

factorial : Int -> Int
factorial n =
    if n < 2 then 1
    else n * factorial (n - 1)

main = factorial 5
