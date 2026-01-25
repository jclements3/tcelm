-- expect: 11
module Test exposing (main)

stringCombine : String -> String -> String
stringCombine a b = a ++ b

stringEmpty : String
stringEmpty = ""

greeting : String
greeting = stringCombine "hello" (stringCombine " " "world")

main = String.length greeting
