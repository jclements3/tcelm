module Rtems.Clock exposing
    ( TimeOfDay
    , getTime
    , setTime
    , getUptime
    , getUptimeNs
    , getTicksPerSecond
    , ticksToMs
    , msToTicks
    , Date
    , getDate
    , isLeapYear
    , daysInMonth
    )

{-| Wall-clock time API for RTEMS.

This module provides access to the real-time clock (RTC) for
calendar time, distinct from the monotonic tick counter.

# Time of Day

@docs TimeOfDay, getTime, setTime

# Uptime

@docs getUptime, getUptimeNs

# Tick Conversion

@docs getTicksPerSecond, ticksToMs, msToTicks

# Date Utilities

@docs Date, getDate, isLeapYear, daysInMonth

-}

import Task exposing (Task)


{-| Time of day with calendar components.
-}
type alias TimeOfDay =
    { year : Int
    , month : Int -- 1-12
    , day : Int -- 1-31
    , hour : Int -- 0-23
    , minute : Int -- 0-59
    , second : Int -- 0-59
    , ticks : Int -- Sub-second ticks
    }


{-| Get the current wall-clock time.

Returns the real-time clock value if set, otherwise returns
a default date (typically 1988-01-01 for RTEMS).

    time <- Clock.getTime
    log ("Current time: " ++ formatTime time)

-}
getTime : Task x TimeOfDay
getTime =
    -- Kernel: rtems_clock_get_tod
    Elm.Kernel.Clock.getTime ()


{-| Set the wall-clock time.

Note: This requires appropriate privileges on most systems.

    Clock.setTime
        { year = 2024
        , month = 3
        , day = 15
        , hour = 14
        , minute = 30
        , second = 0
        , ticks = 0
        }

-}
setTime : TimeOfDay -> Task x ()
setTime tod =
    -- Kernel: rtems_clock_set
    Elm.Kernel.Clock.setTime tod


{-| Get system uptime in seconds.

This is monotonic and not affected by `setTime`.

-}
getUptime : Task x Int
getUptime =
    -- Kernel: rtems_clock_get_uptime
    Elm.Kernel.Clock.getUptime ()


{-| Get system uptime in nanoseconds.

For high-precision timing measurements.

    start <- Clock.getUptimeNs
    doExpensiveWork
    end <- Clock.getUptimeNs
    log ("Work took " ++ String.fromInt (end - start) ++ " ns")

-}
getUptimeNs : Task x Int
getUptimeNs =
    -- Kernel: rtems_clock_get_uptime_nanoseconds
    Elm.Kernel.Clock.getUptimeNs ()


{-| Get the system tick rate (ticks per second).

Common values: 100, 1000, 10000

-}
getTicksPerSecond : Task x Int
getTicksPerSecond =
    -- Kernel: rtems_clock_get_ticks_per_second
    Elm.Kernel.Clock.getTicksPerSecond ()


{-| Convert ticks to milliseconds.
-}
ticksToMs : Int -> Int -> Int
ticksToMs ticksPerSecond ticks =
    (ticks * 1000) // ticksPerSecond


{-| Convert milliseconds to ticks.
-}
msToTicks : Int -> Int -> Int
msToTicks ticksPerSecond ms =
    (ms * ticksPerSecond) // 1000



-- =============================================================================
-- Date Utilities
-- =============================================================================


{-| Date without time components.
-}
type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


{-| Get current date (without time).
-}
getDate : Task x Date
getDate =
    getTime
        |> Task.map
            (\tod ->
                { year = tod.year
                , month = tod.month
                , day = tod.day
                }
            )


{-| Check if a year is a leap year.
-}
isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0)
        && ((modBy 100 year /= 0) || (modBy 400 year == 0))


{-| Get the number of days in a month.
-}
daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        1 ->
            31

        2 ->
            if isLeapYear year then
                29

            else
                28

        3 ->
            31

        4 ->
            30

        5 ->
            31

        6 ->
            30

        7 ->
            31

        8 ->
            31

        9 ->
            30

        10 ->
            31

        11 ->
            30

        12 ->
            31

        _ ->
            0
