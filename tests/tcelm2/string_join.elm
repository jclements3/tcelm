-- expect: 7
module Test exposing (main)

main = String.length (String.join ", " ["a", "b", "c"])
