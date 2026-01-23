module Rtems.Interrupt exposing
    ( onInterrupt
    , InterruptInfo
    , subscribe
    , unsubscribe
    , SubscriptionId
    , getCount
    , resetCount
    )

{-| Interrupt handling for RTEMS.

This module provides a safe way to receive interrupt notifications
in Elm code. Actual ISR code runs in a deferred context, allowing
normal Elm message processing.

# Subscriptions

@docs onInterrupt, subscribe, unsubscribe, SubscriptionId

# Information

@docs InterruptInfo, getCount, resetCount

# Example

    -- Subscribe to interrupt 5
    subscriptions model =
        Rtems.Interrupt.onInterrupt 5 InterruptReceived

    update msg model =
        case msg of
            InterruptReceived info ->
                -- Handle interrupt
                ( { model | count = model.count + 1 }, Cmd.none )

-}

import Task exposing (Task)


{-| Information about an interrupt event.
-}
type alias InterruptInfo =
    { irqNumber : Int
    , timestamp : Int -- Tick count when interrupt occurred
    , data : Int -- Optional data from ISR
    }


{-| Subscription handle for unsubscribing.
-}
type SubscriptionId
    = SubscriptionId Int


{-| Subscribe to an interrupt.

The message is sent whenever the interrupt fires.

    subscriptions model =
        Rtems.Interrupt.onInterrupt 5 InterruptReceived

-}
onInterrupt : Int -> (InterruptInfo -> msg) -> Sub msg
onInterrupt irqNumber toMsg =
    -- Kernel function: tcelm_isr_subscribe
    Elm.Kernel.Interrupt.onInterrupt irqNumber toMsg


{-| Subscribe to an interrupt and get a subscription ID.

Use this when you need to unsubscribe later.

    init =
        ( initialModel
        , Rtems.Interrupt.subscribe 5 GotSubscription
        )

    update msg model =
        case msg of
            GotSubscription subId ->
                ( { model | interruptSub = Just subId }, Cmd.none )

-}
subscribe : Int -> (SubscriptionId -> msg) -> Cmd msg
subscribe irqNumber toMsg =
    -- Kernel function: tcelm_isr_subscribe
    Elm.Kernel.Interrupt.subscribe irqNumber
        |> Task.map SubscriptionId
        |> Task.perform toMsg


{-| Unsubscribe from an interrupt.
-}
unsubscribe : SubscriptionId -> Cmd msg
unsubscribe (SubscriptionId subId) =
    -- Kernel function: tcelm_isr_unsubscribe
    Elm.Kernel.Interrupt.unsubscribe subId


{-| Get the number of times an interrupt has fired.
-}
getCount : SubscriptionId -> Task x Int
getCount (SubscriptionId subId) =
    -- Kernel function: tcelm_isr_get_count
    Elm.Kernel.Interrupt.getCount subId


{-| Reset the interrupt counter.
-}
resetCount : SubscriptionId -> Task x ()
resetCount (SubscriptionId subId) =
    -- Kernel function: tcelm_isr_reset_count
    Elm.Kernel.Interrupt.resetCount subId
