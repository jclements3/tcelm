module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.andThen (\x -> if x > 5 then Just (x * 2) else Nothing) (Just 10))
