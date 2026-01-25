-- expect: "Hello World! World is great."
module Test exposing (main)

main =
    String.replace "Elm" "World" "Hello Elm! Elm is great."
