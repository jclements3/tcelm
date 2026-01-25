-- expect: 10
module Test exposing (main)

double : Int -> Int
double x = x * 2

result : Result String Int
result = Result.map double (Ok 5)

main =
    case result of
        Ok n -> n
        Err _ -> 0
