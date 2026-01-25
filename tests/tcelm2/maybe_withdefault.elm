-- expect: 10
module Test exposing (main)
main = Maybe.withDefault 0 (Just 10)
