-- expect: 55
module Test exposing (main)

double : Int -> Int
double x = x * 2

triple : Int -> Int
triple x = x * 3

add : Int -> Int -> Int
add x y = x + y

combine : Int -> Int
combine n = add (double n) (triple n)

main = combine 10 + 5
