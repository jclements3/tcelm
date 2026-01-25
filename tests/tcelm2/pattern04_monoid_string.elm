-- expect: 11
module Test exposing (main)

stringCombine : String -> String -> String
stringCombine a b = String.append a b

stringEmpty : String
stringEmpty = ""

greeting : String
greeting = stringCombine "hello" (stringCombine " " "world")

main = String.length greeting
