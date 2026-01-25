-- expect: 1
module Test exposing (main)

mfilter : (a -> Bool) -> Maybe a -> Maybe a
mfilter pred ma =
    ma |> Maybe.andThen (\a -> if pred a then Just a else Nothing)

msum : List (Maybe a) -> Maybe a
msum maybes =
    List.foldl (\ma acc ->
        case acc of
            Just _ -> acc
            Nothing -> ma
    ) Nothing maybes

result : Maybe Int
result = msum [ Nothing, Just 1, Just 2 ]

filtered : Maybe Int
filtered = mfilter (\x -> x > 5) (Just 3)

main = Maybe.withDefault 0 result
