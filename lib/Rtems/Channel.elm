module Rtems.Channel exposing
    ( Channel
    , create
    , createWithCapacity
    , send
    , sendTimeout
    , trySend
    , receive
    , receiveTimeout
    , tryReceive
    , broadcast
    , flush
    , pending
    , close
    , sendUrgent
    )

{-| Bounded message-passing channels for RTEMS.

Channels provide typed, bounded queues for inter-task communication.
They wrap RTEMS message queues with priority-aware blocking.

# Creating Channels

@docs Channel, create, createWithCapacity

# Sending Messages

@docs send, sendTimeout, trySend, sendUrgent

# Receiving Messages

@docs receive, receiveTimeout, tryReceive

# Channel Operations

@docs broadcast, flush, pending, close

# Example

    -- Producer task
    channel <- Channel.create
    Channel.send channel "Hello"
    Channel.send channel "World"

    -- Consumer task
    msg1 <- Channel.receive channel  -- "Hello"
    msg2 <- Channel.receive channel  -- "World"

-}

import Task exposing (Task)


-- =============================================================================
-- FFI Declarations
-- =============================================================================

foreign import tcelm_channel_create_default : Int -> Ptr

foreign import tcelm_channel_send : Ptr -> a -> Int

foreign import tcelm_channel_send_timeout : Ptr -> a -> Int -> Int

foreign import tcelm_channel_try_send : Ptr -> a -> Int

foreign import tcelm_channel_receive : Ptr -> a

foreign import tcelm_channel_receive_timeout : Ptr -> Int -> a

foreign import tcelm_channel_try_receive : Ptr -> a

foreign import tcelm_channel_broadcast : Ptr -> a -> Int

foreign import tcelm_channel_flush : Ptr -> Int

foreign import tcelm_channel_pending : Ptr -> Int

foreign import tcelm_channel_close : Ptr -> ()

foreign import tcelm_channel_send_urgent : Ptr -> a -> Int


-- =============================================================================
-- Types
-- =============================================================================


{-| A bounded message channel.

Channels have a fixed capacity. Sends block when full, receives block when empty.

-}
type Channel a
    = Channel Ptr


-- =============================================================================
-- Creating Channels
-- =============================================================================


{-| Create a channel with default capacity (16 messages).

    channel <- Channel.create

-}
create : Task x (Channel a)
create =
    createWithCapacity 16


{-| Create a channel with specified capacity.

    -- Create a small channel for control messages
    controlChannel <- Channel.createWithCapacity 4

    -- Create a larger channel for data
    dataChannel <- Channel.createWithCapacity 256

-}
createWithCapacity : Int -> Task x (Channel a)
createWithCapacity capacity =
    Task.succeed (Channel (tcelm_channel_create_default capacity))


-- =============================================================================
-- Sending Messages
-- =============================================================================


{-| Send a message to the channel.

Blocks if the channel is full until space is available.

    Channel.send channel "Hello"

-}
send : Channel a -> a -> Task x ()
send (Channel ptr) msg =
    let
        result = tcelm_channel_send ptr msg
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail ()


{-| Send a message with timeout in milliseconds.

Returns `Err Timeout` if the channel remains full for the duration.

    case Channel.sendTimeout 1000 channel msg of
        Ok () -> log "Sent"
        Err _ -> log "Channel full for 1 second"

-}
sendTimeout : Int -> Channel a -> a -> Task SendError ()
sendTimeout timeoutMs (Channel ptr) msg =
    let
        result = tcelm_channel_send_timeout ptr msg timeoutMs
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail Timeout


{-| Error type for send operations.
-}
type SendError
    = Timeout
    | ChannelFull


{-| Try to send without blocking.

Returns immediately with `Err ChannelFull` if no space available.

    case Channel.trySend channel msg of
        Ok () -> log "Sent immediately"
        Err ChannelFull -> log "Would block, doing something else"

-}
trySend : Channel a -> a -> Task SendError ()
trySend (Channel ptr) msg =
    let
        result = tcelm_channel_try_send ptr msg
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail ChannelFull


{-| Send an urgent message that goes to the front of the queue.

Use sparingly for high-priority messages that should be processed first.

    -- Send shutdown signal that jumps the queue
    Channel.sendUrgent channel Shutdown

-}
sendUrgent : Channel a -> a -> Task x ()
sendUrgent (Channel ptr) msg =
    let
        result = tcelm_channel_send_urgent ptr msg
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail ()


-- =============================================================================
-- Receiving Messages
-- =============================================================================


{-| Receive a message from the channel.

Blocks until a message is available.

    msg <- Channel.receive channel

-}
receive : Channel a -> Task x a
receive (Channel ptr) =
    Task.succeed (tcelm_channel_receive ptr)


{-| Receive with timeout in milliseconds.

Returns `Nothing` if no message arrives within the timeout.

    case Channel.receiveTimeout 5000 channel of
        Just msg -> process msg
        Nothing -> log "No message in 5 seconds"

-}
receiveTimeout : Int -> Channel a -> Task x (Maybe a)
receiveTimeout timeoutMs (Channel ptr) =
    Task.succeed (tcelm_channel_receive_timeout ptr timeoutMs)


{-| Try to receive without blocking.

Returns `Nothing` if no message is available.

    case Channel.tryReceive channel of
        Just msg -> process msg
        Nothing -> doSomethingElse

-}
tryReceive : Channel a -> Task x (Maybe a)
tryReceive (Channel ptr) =
    Task.succeed (tcelm_channel_try_receive ptr)


-- =============================================================================
-- Channel Operations
-- =============================================================================


{-| Broadcast a message to all waiting receivers.

Returns the number of tasks that received the message.

    count <- Channel.broadcast channel "Everyone listen!"
    log (String.fromInt count ++ " tasks received the broadcast")

-}
broadcast : Channel a -> a -> Task x Int
broadcast (Channel ptr) msg =
    Task.succeed (tcelm_channel_broadcast ptr msg)


{-| Flush all pending messages from the channel.

Returns the number of messages discarded.

    count <- Channel.flush channel
    log ("Discarded " ++ String.fromInt count ++ " messages")

-}
flush : Channel a -> Task x Int
flush (Channel ptr) =
    Task.succeed (tcelm_channel_flush ptr)


{-| Get the number of pending (unread) messages.

Note: This value may be stale immediately after the call returns.

-}
pending : Channel a -> Task x Int
pending (Channel ptr) =
    Task.succeed (tcelm_channel_pending ptr)


{-| Close and delete a channel.

Any blocked senders/receivers will be released with errors.

-}
close : Channel a -> Task x ()
close (Channel ptr) =
    let
        _ = tcelm_channel_close ptr
    in
    Task.succeed ()
