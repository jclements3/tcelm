module Rtems.Barrier exposing
    ( Barrier
    , create
    , createWithName
    , wait
    , waitTimeout
    , release
    , getWaiting
    , reset
    , delete
    , WaitResult(..)
    )

{-| Barrier synchronization for RTEMS.

Barriers allow multiple tasks to synchronize at a common point.
All tasks must reach the barrier before any can proceed.

# Use Cases

  - Phase-based computation (all tasks complete phase N before starting N+1)
  - Parallel algorithms requiring synchronization points
  - Initialization barriers (all tasks ready before starting work)

# Example

    -- Three worker tasks synchronize at each phase
    barrier <- Barrier.create 3

    -- In each worker task:
    doPhase1Work
    Barrier.wait barrier  -- All 3 must reach here

    doPhase2Work
    Barrier.wait barrier  -- All 3 must reach here

    doPhase3Work

@docs Barrier, create, createWithName
@docs wait, waitTimeout, WaitResult
@docs release, getWaiting, reset, delete

-}

import Task exposing (Task)


{-| A barrier handle.
-}
type Barrier
    = Barrier


{-| Result of waiting at a barrier.
-}
type WaitResult
    = Success
      -- ^ Successfully synchronized with other tasks
    | Released
      -- ^ This task was the one that released the barrier (the "serial" task)
    | Timeout
      -- ^ Timed out waiting for other tasks
    | Error
      -- ^ An error occurred (barrier deleted, etc.)


{-| Create a barrier for the specified number of tasks.

    -- Create barrier for 4 worker tasks
    barrier <- Barrier.create 4

-}
create : Int -> Task x Barrier
create count =
    -- Kernel: tcelm_barrier_create_with_count
    Elm.Kernel.Barrier.create count


{-| Create a named barrier (useful for debugging).

    barrier <- Barrier.createWithName 4 "worker_sync"

-}
createWithName : Int -> String -> Task x Barrier
createWithName count name =
    -- Kernel: tcelm_barrier_create
    Elm.Kernel.Barrier.createWithName count name


{-| Wait at the barrier.

Blocks until all tasks have called `wait`. When the last task arrives,
all waiting tasks are released simultaneously.

One task (the last to arrive) receives `Released`, all others receive `Success`.
This allows one task to perform cleanup or setup for the next phase.

    result <- Barrier.wait barrier
    case result of
        Released ->
            -- I was the last to arrive, do phase setup
            initializeNextPhase

        Success ->
            -- I was waiting, now released
            ()

        _ ->
            -- Handle error
            ()

-}
wait : Barrier -> Task x WaitResult
wait barrier =
    -- Kernel: tcelm_barrier_wait
    Elm.Kernel.Barrier.wait barrier


{-| Wait with timeout in milliseconds.

If not all tasks arrive within the timeout, returns `Timeout`.

    result <- Barrier.waitTimeout 5000 barrier
    case result of
        Timeout ->
            -- Some tasks didn't arrive in 5 seconds
            handleStalledTask

        _ ->
            -- Normal synchronization
            ()

-}
waitTimeout : Int -> Barrier -> Task x WaitResult
waitTimeout timeoutMs barrier =
    -- Kernel: tcelm_barrier_wait_timeout
    Elm.Kernel.Barrier.waitTimeout timeoutMs barrier


{-| Manually release all waiting tasks.

Use this to abort a barrier wait (e.g., during shutdown).
Returns the number of tasks that were released.

    -- Abort barrier during error handling
    count <- Barrier.release barrier
    log ("Released " ++ String.fromInt count ++ " waiting tasks")

-}
release : Barrier -> Task x Int
release barrier =
    -- Kernel: tcelm_barrier_release
    Elm.Kernel.Barrier.release barrier


{-| Get the number of tasks currently waiting at the barrier.

Note: This value may be stale immediately after the call returns.

-}
getWaiting : Barrier -> Task x Int
getWaiting barrier =
    -- Kernel: tcelm_barrier_get_waiting
    Elm.Kernel.Barrier.getWaiting barrier


{-| Reset the barrier for reuse.

Only safe to call when no tasks are waiting.

-}
reset : Barrier -> Task x ()
reset barrier =
    -- Kernel: tcelm_barrier_reset
    Elm.Kernel.Barrier.reset barrier


{-| Delete a barrier.

Any waiting tasks will be released with `Error`.

-}
delete : Barrier -> Task x ()
delete barrier =
    -- Kernel: tcelm_barrier_delete
    Elm.Kernel.Barrier.delete barrier
