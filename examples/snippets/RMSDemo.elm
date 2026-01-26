module RMSDemo exposing (main)

{-| Simple Rate Monotonic Scheduler Demo

Demonstrates RMS types and deadline status handling.
This example compiles and runs on TCC for testing.

-}


formatDeadline : Int -> String
formatDeadline status =
    if status == 0 then
        "ON_TIME - Deadline met"

    else if status == 1 then
        "MISSED - Deadline violated!"

    else
        "NOT_STARTED - Period not yet active"


computeValue : Int -> Int
computeValue iteration =
    let
        base =
            iteration * 3

        adjustment =
            if iteration > 10 then
                5

            else
                0
    in
    base + adjustment + 42


main : String
main =
    let
        iteration =
            5

        deadline =
            0

        value =
            computeValue iteration

        statusMsg =
            formatDeadline deadline
    in
    "RMS Demo - Iteration: "
        ++ String.fromInt iteration
        ++ ", Value: "
        ++ String.fromInt value
        ++ ", Status: "
        ++ statusMsg
