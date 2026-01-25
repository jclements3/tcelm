-- expect: 60
module Test exposing (main)

add3 : Int -> Int -> Int -> Int
add3 a b c = a + b + c

main = add3 10 20 30
