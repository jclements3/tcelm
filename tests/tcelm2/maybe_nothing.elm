-- expect: 0
module Test exposing (main)
main =
    case Nothing of
        Just n -> n
        Nothing -> 0
