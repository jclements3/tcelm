-- expect: 55
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

v1 : Validation String Int
v1 = validatePositive 5

v2 : Validation String Int
v2 = validateSmall 50

result : Int
result =
    case (v1, v2) of
        (Success a, Success b) -> a + b
        _ -> -1

main = result
