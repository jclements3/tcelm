-- expect: 15
module Test exposing (main)

add a b =
    a + b

main =
    uncurry add (5, 10)
