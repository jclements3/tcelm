-- expect: 20
module Test exposing (main)
main =
    case Maybe.map (\x -> x * 2) (Just 10) of
        Just n -> n
        Nothing -> 0
