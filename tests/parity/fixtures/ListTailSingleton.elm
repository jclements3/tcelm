module Main exposing (main)

-- Check that List.tail of a singleton returns Just (exists)
main : Int
main =
    case List.tail [42] of
        Just _ -> 1
        Nothing -> 0
