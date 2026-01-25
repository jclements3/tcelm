-- expect: 0
module Test exposing (main)

main =
    case Ptr.toMaybe Ptr.null of
        Nothing -> 0
        Just _ -> 1
