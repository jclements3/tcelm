-- expect: "42..."
module Test exposing (main)

main =
    String.padRight 5 '.' "42"
