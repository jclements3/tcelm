module Rtems exposing
    ( -- Time
      now
    , ticks
    , ticksPerSecond
    , every
    , everyTick
    , delay
    , delayTicks

    -- Tasks
    , Task
    , TaskId
    , spawn
    , send
    , self
    , kill

    -- Channels
    , Channel
    , channel
    , subscribe
    , unsubscribe
    , publish

    -- Platform
    , program
    , worker
    )

{-| RTEMS integration for Elm.

This module provides Elm bindings to RTEMS primitives, enabling real-time
functional programming on RTEMS systems.

# Time

@docs now, ticks, ticksPerSecond, every, everyTick, delay, delayTicks

# Tasks

RTEMS tasks are modeled as Elm "actors" that follow The Elm Architecture.

@docs Task, TaskId, spawn, send, self, kill

# Channels

Pub/sub communication between Elm tasks.

@docs Channel, channel, subscribe, unsubscribe, publish

# Platform

@docs program, worker

-}


-- TIME


{-| Get current time in milliseconds since boot.
-}
now : () -> Int
now () =
    -- Kernel function: tcelm_rtems_time_ms
    Elm.Kernel.Rtems.now ()


{-| Get current tick count.
-}
ticks : () -> Int
ticks () =
    -- Kernel function: tcelm_rtems_tick_get
    Elm.Kernel.Rtems.ticks ()


{-| Get system ticks per second.
-}
ticksPerSecond : () -> Int
ticksPerSecond () =
    -- Kernel function: tcelm_rtems_ticks_per_second
    Elm.Kernel.Rtems.ticksPerSecond ()


{-| Subscribe to periodic time events in milliseconds.

    subscriptions model =
        Rtems.every 100 Tick  -- 100ms period, 10 Hz

-}
every : Int -> (Int -> msg) -> Sub msg
every intervalMs toMsg =
    -- Kernel function: tcelm_sub_every_ms
    Elm.Kernel.Rtems.every intervalMs toMsg


{-| Subscribe to periodic time events in system ticks.
More precise than milliseconds for real-time applications.

    subscriptions model =
        Rtems.everyTick 4 Tick  -- Every 4 ticks (at 1kHz = 4ms, 250 Hz)

-}
everyTick : Int -> (Int -> msg) -> Sub msg
everyTick intervalTicks toMsg =
    -- Kernel function: tcelm_sub_every_ticks
    Elm.Kernel.Rtems.everyTick intervalTicks toMsg


{-| Send a delayed message to self.
-}
delay : Int -> msg -> Cmd msg
delay delayMs msg =
    -- Kernel function: tcelm_sub_delay_ms
    Elm.Kernel.Rtems.delay delayMs msg


{-| Send a delayed message in ticks.
-}
delayTicks : Int -> msg -> Cmd msg
delayTicks ticks_ msg =
    -- Kernel function: tcelm_sub_delay_ticks
    Elm.Kernel.Rtems.delayTicks ticks_ msg



-- TASKS


{-| An RTEMS task running Elm code.
-}
type Task msg model
    = Task


{-| A handle to an Elm task for sending messages.
-}
type TaskId msg
    = TaskId


{-| Spawn a new Elm task.

    spawn
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

-}
spawn :
    { init : model
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Cmd (TaskId msg)
spawn config =
    -- Kernel function: tcelm_task_spawn
    Elm.Kernel.Rtems.spawn config


{-| Send a message to another Elm task.
-}
send : TaskId msg -> msg -> Cmd a
send taskId msg =
    -- Kernel function: tcelm_task_send
    Elm.Kernel.Rtems.send taskId msg


{-| Get the current task's ID.
-}
self : () -> TaskId msg
self () =
    -- Kernel function: tcelm_task_self
    Elm.Kernel.Rtems.self ()


{-| Kill an Elm task.
-}
kill : TaskId msg -> Cmd a
kill taskId =
    -- Kernel function: tcelm_task_delete
    Elm.Kernel.Rtems.kill taskId



-- CHANNELS


{-| A named pub/sub channel for inter-task communication.
-}
type Channel msg
    = Channel String


{-| Create or get a named channel.

    sensorChannel : Channel SensorReading
    sensorChannel =
        Rtems.channel "sensors"

-}
channel : String -> Channel msg
channel name =
    Channel name


{-| Subscribe to a channel.
-}
subscribe : Channel msg -> Sub msg
subscribe (Channel name) =
    -- Kernel function: tcelm_channel_subscribe
    Elm.Kernel.Rtems.subscribe name


{-| Unsubscribe from a channel.
-}
unsubscribe : Channel msg -> Cmd a
unsubscribe (Channel name) =
    -- Kernel function: tcelm_channel_unsubscribe
    Elm.Kernel.Rtems.unsubscribe name


{-| Publish a message to all subscribers of a channel.
-}
publish : Channel msg -> msg -> Cmd a
publish (Channel name) msg =
    -- Kernel function: tcelm_channel_publish
    Elm.Kernel.Rtems.publish name msg



-- PLATFORM


{-| Create an Elm program for RTEMS with view output.

    main =
        Rtems.program
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }

-}
program :
    { init : model
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> String
    }
    -> Program () model msg
program config =
    -- This gets compiled to a tcelm_task_def_t
    Elm.Kernel.Rtems.program config


{-| Create an Elm worker (no view) for RTEMS.

    main =
        Rtems.worker
            { init = init
            , update = update
            , subscriptions = subscriptions
            }

-}
worker :
    { init : model
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program () model msg
worker config =
    -- This gets compiled to a tcelm_task_def_t
    Elm.Kernel.Rtems.worker config
