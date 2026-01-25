-- expect: 22
module Test exposing (main)

safeParseInt : String -> Result String Int
safeParseInt s =
    case String.toInt s of
        Just n -> Ok n
        Nothing -> Err "parse error"

safeDivide : Int -> Int -> Result String Int
safeDivide x y =
    if y == 0 then
        Err "division by zero"
    else
        Ok (x // y)

compute : String -> Result String Int
compute input =
    safeParseInt input
        |> Result.andThen (\n -> safeDivide n 2)
        |> Result.map (\n -> n + 1)

result : Result String Int
result = compute "42"

main =
    case result of
        Ok n -> n
        Err _ -> -1
