-- expect: 42
module Test exposing (main)

getValue : Maybe (Maybe Int) -> Int
getValue outer =
    case outer of
        Nothing -> 0
        Just inner ->
            case inner of
                Nothing -> 1
                Just n -> n

main = getValue (Just (Just 42))
