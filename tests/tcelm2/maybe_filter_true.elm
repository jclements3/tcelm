-- expect: Just 10
module Test exposing (main)

main =
    Maybe.filter (\x -> x > 5) (Just 10)
