-- expect: Just ('H', "ello")
module Test exposing (main)

main =
    String.uncons "Hello"
