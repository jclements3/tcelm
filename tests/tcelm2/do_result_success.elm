-- expect: 30
module Test exposing (main)

validate : Int -> Result String Int
validate n = if n > 0 then Ok n else Err "invalid"

main =
    case do
        x <- validate 10
        y <- validate 20
        Ok (x + y)
    of
        Ok n -> n
        Err _ -> 0
