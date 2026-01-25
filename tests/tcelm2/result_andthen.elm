-- expect: 30
module Test exposing (main)

validate : Int -> Result String Int
validate n = if n > 0 then Ok n else Err "invalid"

main =
    case Result.andThen (\x -> Ok (x + 10)) (validate 20) of
        Ok n -> n
        Err _ -> 0
