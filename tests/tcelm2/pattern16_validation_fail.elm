-- expect: -1
module Test exposing (main)

type Validation e a
    = Success a
    | Failure (List e)

validatePositive : Int -> Validation String Int
validatePositive n =
    if n > 0 then
        Success n
    else
        Failure [ "not positive" ]

validateSmall : Int -> Validation String Int
validateSmall n =
    if n < 100 then
        Success n
    else
        Failure [ "too large" ]

negFive : Int
negFive = 0 - 5

v1 : Validation String Int
v1 = validatePositive negFive

v2 : Validation String Int
v2 = validateSmall 150

result : Int
result =
    case (v1, v2) of
        (Success a, Success b) -> a + b
        _ -> 0 - 1

main = result
