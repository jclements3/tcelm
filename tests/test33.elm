module Test exposing (main)


validate : Int -> Result String Int
validate n =
    if n > 0 then
        Ok n
    else
        Err "must be positive"


validatePositives : Int -> Int -> Result String Int
validatePositives a b = do
    x <- validate a
    y <- validate b
    Ok (x + y)


main : Int
main =
    let
        x = 5
        validResult =
            case validatePositives 10 20 of
                Ok n -> n
                Err _ -> 0
    in
    validResult + x
