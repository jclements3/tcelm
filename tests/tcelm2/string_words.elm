-- expect: ["hello", "world", "test"]
module Test exposing (main)

main =
    String.words "hello world test"
