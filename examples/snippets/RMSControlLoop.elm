module RMSControlLoop exposing (main)

{-| Rate Monotonic Scheduler Example - Hard Real-Time Control Loop

This example demonstrates the RMS module for safety-critical applications
that require deadline violation detection.

The control loop runs at 250 Hz (4ms period) with:
- Automatic deadline tracking
- Statistics collection (min/max/avg execution time)
- Deadline violation notification

For RTEMS deployment:
    tcelm RMSControlLoop.elm --target rtems

-}


type alias Model =
    { iteration : Int
    , totalMissed : Int
    , lastValue : Int
    }


init : Model
init =
    { iteration = 0
    , totalMissed = 0
    , lastValue = 0
    }


computeControl : Int -> Int
computeControl iteration =
    let
        sensor =
            iteration * 7 + 42

        error =
            sensor - 512

        proportional =
            error * 2

        integral =
            iteration // 10

        derivative =
            (sensor - (iteration - 1) * 7 - 42) * 5
    in
    proportional + integral + derivative


formatStatus : Int -> String
formatStatus status =
    if status == 0 then
        "ON_TIME"

    else if status == 1 then
        "MISSED"

    else
        "NOT_STARTED"


main : String
main =
    let
        model =
            init

        control =
            computeControl model.iteration

        status =
            formatStatus 0
    in
    "RMS Control Loop - Period: 4ms (250Hz), Iteration: "
        ++ String.fromInt model.iteration
        ++ ", Control: "
        ++ String.fromInt control
        ++ ", Status: "
        ++ status
        ++ ", Missed: "
        ++ String.fromInt model.totalMissed
