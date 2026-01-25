-- expect: [0, 2, 4, 6, 8]
module Test exposing (main)

main =
    Array.toList (Array.initialize 5 (\i -> i * 2))
