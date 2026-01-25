-- expect: 10
module Test exposing (main)
main =
    case Just 10 of
        Just n -> n
        Nothing -> 0
