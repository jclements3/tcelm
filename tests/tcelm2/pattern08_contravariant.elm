-- expect: 1
module Test exposing (main)

type alias Predicate a = a -> Bool

contramap : (b -> a) -> Predicate a -> Predicate b
contramap f pred =
    \b -> pred (f b)

isEven : Predicate Int
isEven n = modBy 2 n == 0

isEvenLength : Predicate String
isEvenLength = contramap String.length isEven

test1 : Bool
test1 = isEvenLength "hello"

test2 : Bool
test2 = isEvenLength "hi"

main = if test2 && not test1 then 1 else 0
