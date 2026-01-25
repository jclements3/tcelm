-- expect: 10
module Test exposing (main)

example1 : Result String Int
example1 = Ok 5 |> Result.map (\x -> x * 2)

main =
    case example1 of
        Ok n -> n
        Err _ -> -1
