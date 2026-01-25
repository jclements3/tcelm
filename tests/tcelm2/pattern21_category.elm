-- expect: 12
module Test exposing (main)

compose : (b -> c) -> (a -> b) -> (a -> c)
compose f g = f << g

double : Int -> Int
double x = x * 2

addOne : Int -> Int
addOne x = x + 1

combined : Int -> Int
combined = compose double addOne

result : Int
result = combined 5

main = result
