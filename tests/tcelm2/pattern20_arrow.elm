-- expect: 10
module Test exposing (main)

fanout : (a -> b) -> (a -> c) -> a -> (b, c)
fanout f g x = ( f x, g x )

double : Int -> Int
double x = x * 2

result : (Int, String)
result = fanout double String.fromInt 5

main = Tuple.first result
