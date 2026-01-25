-- expect: 25
module Test exposing (main)

mul : Int -> Int -> Int
mul = (*)

main = mul 5 5
