-- expect: [42, 42, 42]
module Test exposing (main)

main =
    Array.toList (Array.repeat 3 42)
