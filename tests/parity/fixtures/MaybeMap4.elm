module Main exposing (main)

main : Int
main =
    Maybe.withDefault 0 (Maybe.map4 (\a b c d -> a + b + c + d) (Just 10) (Just 11) (Just 12) (Just 9))
