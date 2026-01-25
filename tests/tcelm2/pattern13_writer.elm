-- expect: 16
module Test exposing (main)

pure : a -> ( a, List String )
pure x = ( x, [] )

addWithLog : Int -> Int -> ( Int, List String )
addWithLog x y = ( x + y, [ "Added" ] )

multiplyWithLog : Int -> Int -> ( Int, List String )
multiplyWithLog x y = ( x * y, [ "Multiplied" ] )

step1 : ( Int, List String )
step1 = addWithLog 5 3

step2 : ( Int, List String )
step2 =
    let
        (v1, l1) = step1
        (v2, l2) = multiplyWithLog v1 2
    in
    (v2, l1 ++ l2)

resultValue : Int
resultValue = Tuple.first step2

main = resultValue
