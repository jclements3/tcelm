-- expect: 20
module Test exposing (main)

add : Int -> Int -> Int
add x y = x + y

mul : Int -> Int -> Int
mul x y = x * y

computation : Int
computation =
    let
        sum = add 2 3
        result = mul sum 4
    in
    result

main = computation
