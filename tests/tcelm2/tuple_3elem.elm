-- expect: 6
module Test exposing (main)

sum3 : (Int, Int, Int) -> Int
sum3 tuple =
    case tuple of
        (a, b, c) -> a + b + c

main = sum3 (1, 2, 3)
