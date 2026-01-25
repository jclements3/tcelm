-- expect: 1
module Test exposing (main)

productMaybe : Maybe a -> Maybe b -> Maybe (a, b)
productMaybe = Maybe.map2 Tuple.pair

result : Maybe (Int, String)
result = productMaybe (Just 1) (Just "hello")

main =
    case result of
        Just (n, _) -> n
        Nothing -> 0
