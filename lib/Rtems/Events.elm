module Rtems.Events exposing
    ( EventSet
    , empty
    , event
    , union
    , intersection
    , member
    , send
    , receive
    , receiveAny
    , receiveAll
    , receiveTimeout
    , tryReceive
    , WaitMode(..)
    , pending
    , clear
    )

{-| Event flags for RTEMS.

Events provide lightweight task-to-task signaling using bit flags.
Each task has a 32-bit event set that other tasks can signal.

Events are more efficient than message queues when:

  - You only need to signal (no data payload)
  - You need to wait on multiple conditions
  - You want to combine multiple signals

# Event Sets

@docs EventSet, empty, event, union, intersection, member

# Sending Events

@docs send

# Receiving Events

@docs receive, receiveAny, receiveAll, receiveTimeout, tryReceive, WaitMode

# Querying

@docs pending, clear

# Example

    -- Define event bits
    dataReady = Events.event 0
    timeout = Events.event 1
    shutdown = Events.event 2

    -- Wait for data or timeout
    events <- Events.receive (Events.union dataReady timeout) Events.Any
    if Events.member dataReady events then
        processData
    else if Events.member timeout events then
        handleTimeout

-}

import Task exposing (Task)


{-| A set of event flags (32 bits).
-}
type EventSet
    = EventSet Int


{-| Empty event set (no events).
-}
empty : EventSet
empty =
    EventSet 0


{-| Create an event set with a single event bit set.

Event numbers are 0-31.

    dataReady = Events.event 0   -- Bit 0
    timeout = Events.event 1     -- Bit 1
    shutdown = Events.event 31   -- Bit 31

-}
event : Int -> EventSet
event n =
    if n >= 0 && n < 32 then
        EventSet (Bitwise.shiftLeftBy n 1)

    else
        EventSet 0


{-| Combine two event sets (bitwise OR).

    waitFor = Events.union dataReady (Events.union timeout shutdown)

-}
union : EventSet -> EventSet -> EventSet
union (EventSet a) (EventSet b) =
    EventSet (Bitwise.or a b)


{-| Intersection of two event sets (bitwise AND).
-}
intersection : EventSet -> EventSet -> EventSet
intersection (EventSet a) (EventSet b) =
    EventSet (Bitwise.and a b)


{-| Check if an event is in the set.

    if Events.member dataReady receivedEvents then
        handleDataReady

-}
member : EventSet -> EventSet -> Bool
member (EventSet test) (EventSet set) =
    Bitwise.and test set /= 0


{-| How to wait for multiple events.
-}
type WaitMode
    = Any
      -- ^ Return when ANY of the requested events are received
    | All
      -- ^ Return only when ALL requested events are received


{-| Send events to a task.

    -- Signal task that data is ready
    Events.send targetTaskId dataReady

-}
send : Int -> EventSet -> Task x ()
send taskId (EventSet events) =
    -- Kernel: rtems_event_send
    Elm.Kernel.Events.send taskId events


{-| Receive events (blocking).

Waits for the specified events according to the wait mode.
Returns the events that were received (may include unrequested events).

The received events are automatically cleared from the task's event set.

    -- Wait for either data or timeout
    received <- Events.receive (Events.union dataReady timeout) Events.Any

-}
receive : EventSet -> WaitMode -> Task x EventSet
receive (EventSet requested) mode =
    let
        modeFlag =
            case mode of
                Any ->
                    0

                All ->
                    1
    in
    -- Kernel: rtems_event_receive
    Elm.Kernel.Events.receive requested modeFlag 0
        |> Task.map EventSet


{-| Wait for any of the specified events.

Shorthand for `receive events Any`.

-}
receiveAny : EventSet -> Task x EventSet
receiveAny events =
    receive events Any


{-| Wait for all of the specified events.

Shorthand for `receive events All`.

-}
receiveAll : EventSet -> Task x EventSet
receiveAll events =
    receive events All


{-| Receive with timeout in milliseconds.

Returns `Nothing` on timeout, `Just events` on success.

    result <- Events.receiveTimeout 1000 dataReady Events.Any
    case result of
        Just received -> handleEvents received
        Nothing -> handleTimeout

-}
receiveTimeout : Int -> EventSet -> WaitMode -> Task x (Maybe EventSet)
receiveTimeout timeoutMs (EventSet requested) mode =
    let
        modeFlag =
            case mode of
                Any ->
                    0

                All ->
                    1
    in
    -- Kernel: rtems_event_receive with timeout
    Elm.Kernel.Events.receiveTimeout requested modeFlag timeoutMs
        |> Task.map
            (\result ->
                if result < 0 then
                    Nothing

                else
                    Just (EventSet result)
            )


{-| Try to receive events without blocking.

Returns immediately with whatever events are pending that match the request.
Returns `empty` if no matching events are pending.

-}
tryReceive : EventSet -> WaitMode -> Task x EventSet
tryReceive (EventSet requested) mode =
    let
        modeFlag =
            case mode of
                Any ->
                    0

                All ->
                    1
    in
    -- Kernel: rtems_event_receive with RTEMS_NO_WAIT
    Elm.Kernel.Events.tryReceive requested modeFlag
        |> Task.map EventSet


{-| Get pending events without consuming them.

Returns the events currently pending for the calling task.

-}
pending : Task x EventSet
pending =
    -- Kernel: Check pending without consuming
    Elm.Kernel.Events.pending ()
        |> Task.map EventSet


{-| Clear specific events from the pending set.

    -- Clear the data ready flag
    Events.clear dataReady

-}
clear : EventSet -> Task x ()
clear (EventSet events) =
    -- Kernel: rtems_event_receive with RTEMS_NO_WAIT to consume
    Elm.Kernel.Events.clear events



-- =============================================================================
-- Bitwise (inline for self-hosting)
-- =============================================================================


type alias Bitwise =
    { shiftLeftBy : Int -> Int -> Int
    , or : Int -> Int -> Int
    , and : Int -> Int -> Int
    }


Bitwise =
    { shiftLeftBy = \n x -> Elm.Kernel.Basics.shiftLeftBy n x
    , or = \a b -> Elm.Kernel.Basics.or a b
    , and = \a b -> Elm.Kernel.Basics.and a b
    }
