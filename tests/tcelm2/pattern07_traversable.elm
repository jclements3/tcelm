-- expect: 12
module Test exposing (main)

traverseMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
traverseMaybe f list =
    let
        step item acc =
            case acc of
                Nothing -> Nothing
                Just collected ->
                    case f item of
                        Nothing -> Nothing
                        Just val -> Just (collected ++ [val])
    in
    List.foldl step (Just []) list

safeDouble : Int -> Maybe Int
safeDouble x =
    if x < 0 then Nothing else Just (x * 2)

result : Maybe (List Int)
result = traverseMaybe safeDouble [1, 2, 3]

main =
    case result of
        Just nums -> List.sum nums
        Nothing -> -1
