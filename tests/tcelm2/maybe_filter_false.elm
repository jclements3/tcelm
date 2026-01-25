-- expect: Nothing
module Test exposing (main)

main =
    Maybe.filter (\x -> x > 15) (Just 10)
