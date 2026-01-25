-- expect: ["a", "b", "c"]
module Test exposing (main)

main =
    String.split "," "a,b,c"
