-- expect: 80
module Test exposing (main)

makeMultiplierAdder : Int -> Int -> Int -> Int
makeMultiplierAdder multiplier offset value =
    (value * multiplier) + offset

main =
    let
        doubleAndAdd10 = makeMultiplierAdder 2 10
    in
    [1, 2, 3, 4, 5]
        |> List.map doubleAndAdd10
        |> List.sum
