-- expect: -1
module Test exposing (main)

safeParseInt : String -> Result String Int
safeParseInt s =
    case String.toInt s of
        Just n -> Ok n
        Nothing -> Err "parse error"

result : Result String Int
result = safeParseInt "abc" |> Result.map (\x -> x * 2)

main =
    case result of
        Ok n -> n
        Err _ -> -1
