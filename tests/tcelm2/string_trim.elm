-- expect: "hello"
module Test exposing (main)

main =
    String.trim "  hello  "
