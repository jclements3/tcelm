module Rtems.Task exposing
    ( TaskId
    , self
    , suspend
    , resume
    , restart
    , delete
    , setPriority
    , getPriority
    , setName
    , getName
    , getStatus
    , TaskStatus(..)
    , TaskInfo
    , getInfo
    , yield
    , wakeAfter
    , wakeAfterTicks
    )

{-| Task management for RTEMS.

This module provides extended task control beyond the basic spawn/send
in the main Rtems module.

# Task Identification

@docs TaskId, self

# Task Control

@docs suspend, resume, restart, delete, yield

# Priority

@docs setPriority, getPriority

# Naming

@docs setName, getName

# Status

@docs TaskStatus, TaskInfo, getStatus, getInfo

# Delays

@docs wakeAfter, wakeAfterTicks

-}

import Task exposing (Task)


{-| A task identifier.
-}
type TaskId
    = TaskId Int


{-| Get the current task's ID.
-}
self : Task x TaskId
self =
    -- Kernel: rtems_task_ident(RTEMS_SELF)
    Elm.Kernel.Task.self ()
        |> Task.map TaskId


{-| Suspend a task.

The task will stop executing until `resume` is called.
Cannot suspend self (use `yield` instead).

    suspend otherTask

-}
suspend : TaskId -> Task x ()
suspend (TaskId id) =
    -- Kernel: rtems_task_suspend
    Elm.Kernel.Task.suspend id


{-| Resume a suspended task.

    resume otherTask

-}
resume : TaskId -> Task x ()
resume (TaskId id) =
    -- Kernel: rtems_task_resume
    Elm.Kernel.Task.resume id


{-| Restart a task from its initial entry point.

The task's priority, stack, and other attributes are preserved.

-}
restart : TaskId -> Task x ()
restart (TaskId id) =
    -- Kernel: rtems_task_restart
    Elm.Kernel.Task.restart id


{-| Delete a task.

The task will be removed and its resources freed.

-}
delete : TaskId -> Task x ()
delete (TaskId id) =
    -- Kernel: rtems_task_delete
    Elm.Kernel.Task.delete id


{-| Set task priority.

Lower numbers = higher priority. Typical range: 1-255.

    setPriority myTask 10  -- High priority

-}
setPriority : TaskId -> Int -> Task x Int
setPriority (TaskId id) priority =
    -- Kernel: rtems_task_set_priority
    -- Returns the old priority
    Elm.Kernel.Task.setPriority id priority


{-| Get task priority.
-}
getPriority : TaskId -> Task x Int
getPriority (TaskId id) =
    -- Kernel: rtems_task_set_priority with RTEMS_CURRENT_PRIORITY
    Elm.Kernel.Task.getPriority id


{-| Set task name (for debugging).

Names are typically 4 characters (RTEMS limitation).

    setName myTask "CTRL"

-}
setName : TaskId -> String -> Task x ()
setName (TaskId id) name =
    -- Kernel: Set task name
    Elm.Kernel.Task.setName id name


{-| Get task name.
-}
getName : TaskId -> Task x String
getName (TaskId id) =
    -- Kernel: Get task name
    Elm.Kernel.Task.getName id



-- =============================================================================
-- Status
-- =============================================================================


{-| Task execution status.
-}
type TaskStatus
    = Ready
      -- ^ Task is ready to run
    | Running
      -- ^ Task is currently executing
    | Blocked
      -- ^ Task is blocked waiting for a resource
    | Suspended
      -- ^ Task is suspended
    | Dormant
      -- ^ Task exists but hasn't started
    | Unknown


{-| Detailed task information.
-}
type alias TaskInfo =
    { id : TaskId
    , name : String
    , status : TaskStatus
    , priority : Int
    , stackSize : Int
    , stackUsed : Int
    }


{-| Get task status.
-}
getStatus : TaskId -> Task x TaskStatus
getStatus (TaskId id) =
    -- Kernel: Check task state
    Elm.Kernel.Task.getStatus id
        |> Task.map intToStatus


intToStatus : Int -> TaskStatus
intToStatus n =
    case n of
        0 ->
            Ready

        1 ->
            Running

        2 ->
            Blocked

        3 ->
            Suspended

        4 ->
            Dormant

        _ ->
            Unknown


{-| Get detailed task information.
-}
getInfo : TaskId -> Task x TaskInfo
getInfo taskId =
    -- Kernel: rtems_task_get_info or similar
    Elm.Kernel.Task.getInfo taskId



-- =============================================================================
-- Delays
-- =============================================================================


{-| Voluntarily yield the CPU to other tasks of the same priority.
-}
yield : Task x ()
yield =
    -- Kernel: rtems_task_wake_after(RTEMS_YIELD_PROCESSOR)
    Elm.Kernel.Task.yield ()


{-| Sleep for the specified number of milliseconds.

    -- Sleep for 100ms
    wakeAfter 100

-}
wakeAfter : Int -> Task x ()
wakeAfter ms =
    -- Kernel: rtems_task_wake_after (converted to ticks)
    Elm.Kernel.Task.wakeAfter ms


{-| Sleep for the specified number of ticks.

More precise than milliseconds for real-time applications.

    -- Sleep for 4 ticks (at 1kHz = 4ms)
    wakeAfterTicks 4

-}
wakeAfterTicks : Int -> Task x ()
wakeAfterTicks ticks =
    -- Kernel: rtems_task_wake_after
    Elm.Kernel.Task.wakeAfterTicks ticks
