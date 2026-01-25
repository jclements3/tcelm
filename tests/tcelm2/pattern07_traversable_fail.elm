-- expect: -1
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

safePositive : Int -> Maybe Int
safePositive x =
    if x < 0 then Nothing else Just x

result : Maybe (List Int)
result = traverseMaybe safePositive [1, -2, 3]

main =
    case result of
        Just nums -> List.sum nums
        Nothing -> -1
