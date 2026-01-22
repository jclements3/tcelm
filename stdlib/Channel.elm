module Channel exposing
    ( Channel
    , create
    , send
    , receive
    , tryReceive
    , broadcast
    , close
    )

{-| Channels provide message-passing communication between tasks.

On RTEMS, Channels map directly to rtems_message_queue_* primitives,
giving you bounded, priority-aware message queues.

# Types
@docs Channel

# Creating Channels
@docs create

# Sending Messages
@docs send, broadcast

# Receiving Messages
@docs receive, tryReceive

# Cleanup
@docs close

-}

import Task exposing (Task)


{-| A Channel is a typed message queue.
On RTEMS, this wraps an rtems_id for a message queue.
-}
type Channel a
    = Channel Int  -- rtems_id


{-| Create a new channel with the given capacity.
On RTEMS, calls rtems_message_queue_create.

    create 10  -- Channel with capacity for 10 messages

-}
create : Int -> Task x (Channel a)
create capacity =
    -- Implemented by compiler as tcelm_channel_create
    create capacity


{-| Send a message to a channel.
On RTEMS, calls rtems_message_queue_send.
Blocks if the queue is full (RTEMS_WAIT).

    send channel "hello"

-}
send : Channel a -> a -> Task x ()
send channel msg =
    -- Implemented by compiler as tcelm_channel_send
    send channel msg


{-| Receive a message from a channel.
On RTEMS, calls rtems_message_queue_receive with RTEMS_WAIT.
Blocks until a message is available.

    receive channel

-}
receive : Channel a -> Task x a
receive channel =
    -- Implemented by compiler as tcelm_channel_receive
    receive channel


{-| Try to receive a message without blocking.
On RTEMS, calls rtems_message_queue_receive with RTEMS_NO_WAIT.
Returns Nothing if no message is available.

    tryReceive channel

-}
tryReceive : Channel a -> Task x (Maybe a)
tryReceive channel =
    -- Implemented by compiler as tcelm_channel_try_receive
    tryReceive channel


{-| Send a message to all waiting receivers.
On RTEMS, calls rtems_message_queue_broadcast.

    broadcast channel "shutdown"

-}
broadcast : Channel a -> a -> Task x Int
broadcast channel msg =
    -- Implemented by compiler as tcelm_channel_broadcast
    broadcast channel msg


{-| Close and delete a channel.
On RTEMS, calls rtems_message_queue_delete.

    close channel

-}
close : Channel a -> Task x ()
close channel =
    -- Implemented by compiler as tcelm_channel_close
    close channel
