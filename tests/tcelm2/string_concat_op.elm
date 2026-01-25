-- expect: 11
module Test exposing (main)

greeting : String
greeting = "hello" ++ " " ++ "world"

main = String.length greeting
