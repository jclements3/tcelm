-- expect: 0
module Test exposing (main)

mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter pred ma =
    ma |> Maybe.andThen (\a -> if pred a then Just a else Nothing)

filtered : Maybe Int
filtered = mfilter (\x -> x > 5) (Just 3)

main = Maybe.withDefault 0 filtered
