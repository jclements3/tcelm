module Main exposing (main)

main : Int
main =
    case Maybe.map (\x -> x * 2) (Just 21) of
        Just n -> n
        Nothing -> 0
