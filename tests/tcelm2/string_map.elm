-- expect: "ABC"
module Test exposing (main)

main =
    String.map Char.toUpper "abc"
