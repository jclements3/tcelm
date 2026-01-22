module Timer exposing
    ( Timer
    , Subscription
    , every
    , delay
    , periodic
    , cancel
    , now
    , ticks
    , ticksPerSecond
    )

{-| Timer provides time-based operations and subscriptions.

On RTEMS, Timers map to rtems_timer_* and rtems_rate_monotonic_*
for deterministic real-time scheduling.

# Types
@docs Timer, Subscription

# Subscriptions
@docs every, delay, periodic

# Control
@docs cancel

# Time Queries
@docs now, ticks, ticksPerSecond

-}

import Task exposing (Task)


{-| A Timer handle.
On RTEMS, wraps rtems_id for a timer or rate monotonic period.
-}
type Timer
    = Timer Int  -- rtems_id


{-| A subscription to time events.
-}
type Subscription msg
    = Every Int (Int -> msg)
    | Delay Int msg
    | Periodic Int (Int -> msg)


{-| Subscribe to events every N milliseconds.
On RTEMS, uses rtems_timer_fire_after with auto-reset.

    every 1000 Tick  -- Receive Tick every second

-}
every : Int -> (Int -> msg) -> Subscription msg
every intervalMs toMsg =
    Every intervalMs toMsg


{-| Fire once after N milliseconds.
On RTEMS, uses rtems_timer_fire_after (one-shot).

    delay 5000 Timeout  -- Receive Timeout after 5 seconds

-}
delay : Int -> msg -> Subscription msg
delay delayMs msg =
    Delay delayMs msg


{-| Periodic execution with rate monotonic scheduling.
On RTEMS, uses rtems_rate_monotonic_period for hard real-time.
This guarantees deterministic execution at fixed intervals.

    periodic 4 SensorUpdate  -- 250 Hz (4ms period) for 240 Hz requirement

-}
periodic : Int -> (Int -> msg) -> Subscription msg
periodic periodMs toMsg =
    Periodic periodMs toMsg


{-| Cancel a timer.
On RTEMS, calls rtems_timer_cancel.

    cancel timer

-}
cancel : Timer -> Task x ()
cancel timer =
    -- Implemented by compiler as tcelm_timer_cancel
    cancel timer


{-| Get current time in milliseconds since boot.
On RTEMS, uses rtems_clock_get_uptime.

    now  -- Task x Int

-}
now : Task x Int
now =
    -- Implemented by compiler as tcelm_timer_now
    now


{-| Get current tick count.
On RTEMS, uses rtems_clock_get_ticks_since_boot.

    ticks  -- Task x Int

-}
ticks : Task x Int
ticks =
    -- Implemented by compiler as tcelm_timer_ticks
    ticks


{-| Get ticks per second (system clock rate).
On RTEMS, uses rtems_clock_get_ticks_per_second.
Typically 1000 (1ms tick) or 100 (10ms tick).

    ticksPerSecond  -- Task x Int

-}
ticksPerSecond : Task x Int
ticksPerSecond =
    -- Implemented by compiler as tcelm_timer_ticks_per_second
    ticksPerSecond
