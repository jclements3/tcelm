module Task exposing
    ( Task
    , succeed
    , fail
    , map
    , map2
    , andThen
    , sequence
    , spawn
    , await
    , parallel
    , race
    , sleep
    , getPriority
    , setPriority
    , setAffinity
    )

{-| Task represents an asynchronous computation that may fail.

On RTEMS, Tasks map directly to rtems_task_* primitives, giving you
real-time guarantees and deterministic scheduling.

# Types
@docs Task

# Creating Tasks
@docs succeed, fail

# Transforming Tasks
@docs map, map2, andThen, sequence

# Running Tasks
@docs spawn, await, parallel, race

# Timing
@docs sleep

# Priority Control
@docs getPriority, setPriority, setAffinity

-}


{-| A Task represents a computation that will be executed asynchronously.
On RTEMS, this wraps an rtems_task_id.
-}
type Task x a
    = Task (TaskHandle x a)


type alias TaskHandle x a =
    { id : Int  -- rtems_id
    , result : Result x a
    }


{-| Create a task that succeeds with the given value.
-}
succeed : a -> Task x a
succeed value =
    -- Implemented by compiler as tcelm_task_succeed
    succeed value


{-| Create a task that fails with the given error.
-}
fail : x -> Task x a
fail error =
    -- Implemented by compiler as tcelm_task_fail
    fail error


{-| Transform the success value of a task.
-}
map : (a -> b) -> Task x a -> Task x b
map fn task =
    -- Implemented by compiler as tcelm_task_map
    map fn task


{-| Combine two tasks.
-}
map2 : (a -> b -> c) -> Task x a -> Task x b -> Task x c
map2 fn taskA taskB =
    -- Implemented by compiler as tcelm_task_map2
    map2 fn taskA taskB


{-| Chain tasks together.
-}
andThen : (a -> Task x b) -> Task x a -> Task x b
andThen fn task =
    -- Implemented by compiler as tcelm_task_andThen
    andThen fn task


{-| Run a list of tasks in sequence, collecting results.
-}
sequence : List (Task x a) -> Task x (List a)
sequence tasks =
    -- Implemented by compiler as tcelm_task_sequence
    sequence tasks


{-| Spawn a task to run concurrently.
On RTEMS, this calls rtems_task_create and rtems_task_start.
Returns a handle that can be used with await.
-}
spawn : (() -> a) -> Task x Int
spawn thunk =
    -- Implemented by compiler as tcelm_task_spawn
    spawn thunk


{-| Wait for a spawned task to complete and get its result.
On RTEMS, this blocks using rtems_event_receive.
-}
await : Int -> Task x a
await taskId =
    -- Implemented by compiler as tcelm_task_await
    await taskId


{-| Run multiple tasks in parallel, waiting for all to complete.
On RTEMS, spawns multiple tasks and waits on a barrier.
-}
parallel : List (Task x a) -> Task x (List a)
parallel tasks =
    -- Implemented by compiler as tcelm_task_parallel
    parallel tasks


{-| Run multiple tasks, returning the first to complete.
On RTEMS, uses rtems_event_receive with RTEMS_EVENT_ANY.
-}
race : List (Task x a) -> Task x a
race tasks =
    -- Implemented by compiler as tcelm_task_race
    race tasks


{-| Sleep for the given number of milliseconds.
On RTEMS, calls rtems_task_wake_after.
-}
sleep : Int -> Task x ()
sleep ms =
    -- Implemented by compiler as tcelm_task_sleep
    sleep ms


{-| Get the current task's priority (lower = higher priority).
On RTEMS, calls rtems_task_get_priority.
-}
getPriority : Task x Int
getPriority =
    -- Implemented by compiler as tcelm_task_get_priority
    getPriority


{-| Set the current task's priority.
On RTEMS, calls rtems_task_set_priority.
-}
setPriority : Int -> Task x ()
setPriority priority =
    -- Implemented by compiler as tcelm_task_set_priority
    setPriority priority


{-| Pin the current task to a specific CPU core (0-3 on NUC).
On RTEMS, calls rtems_task_set_affinity.
-}
setAffinity : Int -> Task x ()
setAffinity core =
    -- Implemented by compiler as tcelm_task_set_affinity
    setAffinity core
