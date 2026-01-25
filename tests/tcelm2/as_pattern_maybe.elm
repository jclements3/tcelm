-- expect: 42
module Test exposing (main)

getValue : Maybe Int -> Int
getValue maybeVal =
    case maybeVal of
        Nothing -> 0
        Just n as whole -> 
            case whole of
                Just x -> x
                Nothing -> 0

main = getValue (Just 42)
