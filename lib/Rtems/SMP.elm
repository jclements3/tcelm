module Rtems.SMP exposing
    ( -- Processor Information
      processorCount
    , currentProcessor
    , isEnabled
    , ProcessorSet
    , allProcessors
    , processor
    , processors

    -- Affinity
    , getAffinity
    , setAffinity
    , getAffinityDefault

    -- Processor Control
    , ProcessorState(..)
    , getProcessorState
    )

{-| SMP (Symmetric Multi-Processing) support for RTEMS.

This module provides CPU affinity control for multi-core systems,
allowing tasks to be pinned to specific processors for:

  - Cache optimization (keep task on same core)
  - Deadline isolation (dedicate core to critical task)
  - Load balancing (spread work across cores)

# Processor Information

@docs processorCount, currentProcessor, isEnabled
@docs ProcessorSet, allProcessors, processor, processors

# Affinity Control

@docs getAffinity, setAffinity, getAffinityDefault

# Processor State

@docs ProcessorState, getProcessorState

# Example

    -- Pin high-priority task to CPU 0
    Rtems.SMP.setAffinity taskId (Rtems.SMP.processor 0)

    -- Allow task to run on CPUs 0 and 1
    Rtems.SMP.setAffinity taskId (Rtems.SMP.processors [0, 1])

    -- Check which CPU we're on
    cpu <- Rtems.SMP.currentProcessor
    log ("Running on CPU " ++ String.fromInt cpu)

-}

import Task exposing (Task)


-- =============================================================================
-- Processor Information
-- =============================================================================


{-| Get the number of processors in the system.

Returns 1 on uniprocessor systems.

-}
processorCount : Task x Int
processorCount =
    -- Kernel: rtems_scheduler_get_processor_maximum
    Elm.Kernel.SMP.processorCount ()


{-| Get the processor the calling task is currently running on.

Returns 0 on uniprocessor systems.

-}
currentProcessor : Task x Int
currentProcessor =
    -- Kernel: rtems_scheduler_get_processor
    Elm.Kernel.SMP.currentProcessor ()


{-| Check if SMP is enabled in this RTEMS build.

Returns False on uniprocessor configurations.

-}
isEnabled : Task x Bool
isEnabled =
    processorCount
        |> Task.map (\count -> count > 1)



-- =============================================================================
-- Processor Sets
-- =============================================================================


{-| A set of processors for affinity control.

Internally represented as a bitmask where bit N represents processor N.

-}
type ProcessorSet
    = ProcessorSet Int


{-| All available processors.

    -- Allow task to run on any CPU
    Rtems.SMP.setAffinity taskId Rtems.SMP.allProcessors

-}
allProcessors : ProcessorSet
allProcessors =
    -- -1 = all bits set (0xFFFFFFFF)
    ProcessorSet -1


{-| A single processor.

    -- Pin to CPU 0
    cpu0 = Rtems.SMP.processor 0

    -- Pin to CPU 2
    cpu2 = Rtems.SMP.processor 2

-}
processor : Int -> ProcessorSet
processor n =
    if n >= 0 && n < 32 then
        ProcessorSet (shiftLeftBy n 1)

    else
        ProcessorSet 0


{-| Multiple specific processors.

    -- Allow CPUs 0, 2, and 3
    cpuSet = Rtems.SMP.processors [0, 2, 3]

-}
processors : List Int -> ProcessorSet
processors cpus =
    let
        toBit n =
            if n >= 0 && n < 32 then
                shiftLeftBy n 1

            else
                0

        combine cpu acc =
            bitwiseOr acc (toBit cpu)
    in
    ProcessorSet (List.foldl combine 0 cpus)



-- =============================================================================
-- Affinity Control
-- =============================================================================


{-| Get the CPU affinity of a task.

Returns the set of processors the task is allowed to run on.

-}
getAffinity : Int -> Task x ProcessorSet
getAffinity taskId =
    -- Kernel: rtems_task_get_affinity
    Elm.Kernel.SMP.getAffinity taskId
        |> Task.map ProcessorSet


{-| Set the CPU affinity of a task.

Restricts which processors the task can run on.

    -- Pin critical task to CPU 0 for predictable latency
    Rtems.SMP.setAffinity criticalTaskId (Rtems.SMP.processor 0)

    -- Allow background task to run on CPUs 1-3
    Rtems.SMP.setAffinity bgTaskId (Rtems.SMP.processors [1, 2, 3])

-}
setAffinity : Int -> ProcessorSet -> Task x ()
setAffinity taskId (ProcessorSet cpuSet) =
    -- Kernel: rtems_task_set_affinity
    Elm.Kernel.SMP.setAffinity taskId cpuSet


{-| Get the default affinity for new tasks.

Typically returns `allProcessors`.

-}
getAffinityDefault : Task x ProcessorSet
getAffinityDefault =
    -- Kernel: Get scheduler default
    Elm.Kernel.SMP.getAffinityDefault ()
        |> Task.map ProcessorSet



-- =============================================================================
-- Processor State
-- =============================================================================


{-| State of a processor.
-}
type ProcessorState
    = Online
      -- ^ Processor is online and running tasks
    | Offline
      -- ^ Processor is offline (powered down or not started)
    | Starting
      -- ^ Processor is in the process of starting up


{-| Get the state of a processor.
-}
getProcessorState : Int -> Task x ProcessorState
getProcessorState cpuIndex =
    -- Kernel: Check processor state
    Elm.Kernel.SMP.getProcessorState cpuIndex
        |> Task.map
            (\state ->
                case state of
                    0 ->
                        Offline

                    1 ->
                        Online

                    2 ->
                        Starting

                    _ ->
                        Offline
            )



-- =============================================================================
-- Bitwise Helpers (for self-hosting)
-- =============================================================================


shiftLeftBy : Int -> Int -> Int
shiftLeftBy n x =
    Elm.Kernel.Basics.shiftLeftBy n x


bitwiseOr : Int -> Int -> Int
bitwiseOr a b =
    Elm.Kernel.Basics.or a b
