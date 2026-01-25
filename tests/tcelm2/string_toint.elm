-- expect: 123
module Test exposing (main)

main =
    case String.toInt "123" of
        Just n -> n
        Nothing -> 0
