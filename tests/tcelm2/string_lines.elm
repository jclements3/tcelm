-- expect: ["line1", "line2", "line3"]
module Test exposing (main)

main =
    String.lines "line1\nline2\nline3"
