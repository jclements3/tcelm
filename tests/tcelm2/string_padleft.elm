-- expect: "00042"
module Test exposing (main)

main =
    String.padLeft 5 '0' "42"
