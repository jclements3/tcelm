module RMS exposing
    ( -- Types
      Period
    , PeriodStats
    , DeadlineStatus(..)

    -- Period Management
    , create
    , delete
    , start
    , stop

    -- Period Execution
    , waitPeriod
    , checkDeadline

    -- Statistics
    , getStats
    , getMissedCount
    , resetStats

    -- Subscriptions
    , periodic
    , onDeadlineMiss

    -- Priority Assignment
    , assignPriority
    , priorityFromPeriod
    )

{-| Rate Monotonic Scheduler (RMS) support for hard real-time applications.

RMS is a fixed-priority scheduling algorithm where tasks with shorter
periods are assigned higher priorities. This module provides:

  - Deterministic periodic execution using RTEMS rate monotonic periods
  - Deadline violation detection and notification
  - Execution time statistics (min/max/avg CPU time per period)
  - Priority assignment based on period (shorter period = higher priority)

## Safety Guarantees

RMS provides schedulability analysis: if the total CPU utilization is below
the Liu & Layland bound (n * (2^(1/n) - 1) for n tasks), all deadlines will
be met. For example:
  - 1 task: 100% utilization
  - 2 tasks: 82.8% utilization
  - 3 tasks: 78.0% utilization
  - Many tasks: ~69.3% utilization

## Example

    -- 240 Hz control loop with deadline monitoring
    type Msg
        = Frame Int
        | DeadlineMissed PeriodStats

    subscriptions model =
        Sub.batch
            [ RMS.periodic 4 Frame              -- 4ms period (250 Hz)
            , RMS.onDeadlineMiss DeadlineMissed -- Notify on missed deadline
            ]

    update msg model =
        case msg of
            Frame tick ->
                -- Must complete within 4ms or deadline is missed
                ( processFrame model, Cmd.none )

            DeadlineMissed stats ->
                -- Handle deadline violation (log, reduce workload, etc.)
                ( { model | deadlines_missed = stats.missedCount }, Cmd.none )

# Types
@docs Period, PeriodStats, DeadlineStatus

# Period Management
@docs create, delete, start, stop

# Period Execution
@docs waitPeriod, checkDeadline

# Statistics
@docs getStats, getMissedCount, resetStats

# Subscriptions
@docs periodic, onDeadlineMiss

# Priority Assignment
@docs assignPriority, priorityFromPeriod

-}

import Task exposing (Task)


-- TYPES


{-| A rate monotonic period handle.
Wraps an RTEMS rtems_id for a rate_monotonic period object.
-}
type Period
    = Period Int


{-| Statistics for a rate monotonic period.
-}
type alias PeriodStats =
    { count : Int
    -- ^ Number of periods completed
    , missedCount : Int
    -- ^ Number of deadline violations
    , minCpuTimeUs : Int
    -- ^ Minimum CPU time used in a single period (microseconds)
    , maxCpuTimeUs : Int
    -- ^ Maximum CPU time used in a single period (microseconds)
    , avgCpuTimeUs : Int
    -- ^ Average CPU time per period (microseconds)
    , periodMs : Int
    -- ^ Configured period in milliseconds
    , lastStatus : DeadlineStatus
    -- ^ Status of the last period
    }


{-| Status of deadline compliance.
-}
type DeadlineStatus
    = OnTime
      -- ^ Period completed within deadline
    | Missed
      -- ^ Deadline was violated (overran period)
    | NotStarted
      -- ^ Period has not been started yet


-- PERIOD MANAGEMENT


{-| Create a new rate monotonic period.

On RTEMS, this calls rtems_rate_monotonic_create to allocate a period
object for deterministic scheduling.

    period <- RMS.create 4  -- 4ms period (250 Hz)

-}
create : Int -> Task x Period
create periodMs =
    -- Kernel: tcelm_rms_create(period_ms)
    Elm.Kernel.RMS.create periodMs


{-| Delete a rate monotonic period and free resources.

On RTEMS, calls rtems_rate_monotonic_delete.

    RMS.delete period

-}
delete : Period -> Task x ()
delete period =
    -- Kernel: tcelm_rms_delete(period_id)
    Elm.Kernel.RMS.delete period


{-| Start or restart a rate monotonic period.

The first call initiates the period. Subsequent calls block until
the next period boundary (or return immediately if deadline was missed).

On RTEMS, calls rtems_rate_monotonic_period.

-}
start : Period -> Task x DeadlineStatus
start period =
    -- Kernel: tcelm_rms_period(period_id)
    Elm.Kernel.RMS.start period


{-| Stop (cancel) a rate monotonic period.

On RTEMS, calls rtems_rate_monotonic_cancel.

-}
stop : Period -> Task x ()
stop period =
    -- Kernel: tcelm_rms_cancel(period_id)
    Elm.Kernel.RMS.stop period


-- PERIOD EXECUTION


{-| Wait for the next period boundary.

This is the core RMS operation. Call this at the START of each iteration
in your periodic loop. It will:

  1. Block until the next period begins (if called early)
  2. Return OnTime if the deadline was met
  3. Return Missed if the previous period overran its deadline

**Critical:** If Missed is returned, the task has violated its deadline
and may be affecting system schedulability.

    loop : Period -> Model -> Task x ()
    loop period model =
        RMS.waitPeriod period
            |> Task.andThen
                (\status ->
                    case status of
                        RMS.OnTime ->
                            -- Normal execution
                            doWork model
                                |> Task.andThen (\newModel -> loop period newModel)

                        RMS.Missed ->
                            -- Deadline violation! Log and continue
                            logDeadlineMiss model
                                |> Task.andThen (\_ -> loop period model)

                        RMS.NotStarted ->
                            -- First iteration
                            loop period model
                )

-}
waitPeriod : Period -> Task x DeadlineStatus
waitPeriod period =
    -- Kernel: tcelm_rms_wait_period(period_id)
    Elm.Kernel.RMS.waitPeriod period


{-| Check deadline status without blocking.

Returns the current deadline status. Use this to check if the previous
period completed on time without waiting for the next period.

    status <- RMS.checkDeadline period
    case status of
        RMS.Missed -> logWarning "Deadline violated"
        _ -> pure ()

-}
checkDeadline : Period -> Task x DeadlineStatus
checkDeadline period =
    -- Kernel: tcelm_rms_check_status(period_id)
    Elm.Kernel.RMS.checkDeadline period


-- STATISTICS


{-| Get comprehensive statistics for a rate monotonic period.

Returns execution time statistics useful for:
  - Verifying worst-case execution time (WCET) assumptions
  - Monitoring CPU utilization per task
  - Detecting gradual performance degradation
  - Validating schedulability analysis

    stats <- RMS.getStats period
    if stats.maxCpuTimeUs > stats.periodMs * 1000 then
        -- WCET exceeds period! Task is not schedulable
        logError "Task WCET exceeds period"

-}
getStats : Period -> Task x PeriodStats
getStats period =
    -- Kernel: tcelm_rms_get_stats(period_id)
    Elm.Kernel.RMS.getStats period


{-| Get just the missed deadline count.

Lightweight alternative to getStats when only deadline
violations are of interest.

    missed <- RMS.getMissedCount period
    if missed > 0 then
        triggerAlarm ()

-}
getMissedCount : Period -> Task x Int
getMissedCount period =
    -- Kernel: tcelm_rms_get_missed_count(period_id)
    Elm.Kernel.RMS.getMissedCount period


{-| Reset statistics for a period.

Useful for starting a new measurement interval or after
configuration changes.

    RMS.resetStats period

-}
resetStats : Period -> Task x ()
resetStats period =
    -- Kernel: tcelm_rms_reset_stats(period_id)
    Elm.Kernel.RMS.resetStats period


-- SUBSCRIPTIONS


{-| Subscribe to periodic execution with rate monotonic timing.

This is the recommended way to implement periodic tasks in Elm.
The runtime handles period management and deadline tracking automatically.

    subscriptions model =
        RMS.periodic 4 Frame  -- 250 Hz (4ms period)

The Int passed to the message constructor is the tick count at period start.

**Deadline Behavior:** If a deadline is missed, the next period starts
immediately (no accumulated delay). Use `onDeadlineMiss` to be notified.

-}
periodic : Int -> (Int -> msg) -> Sub msg
periodic periodMs toMsg =
    -- Kernel: tcelm_rms_periodic_sub(period_ms, callback)
    Elm.Kernel.RMS.periodic periodMs toMsg


{-| Subscribe to deadline miss notifications.

This subscription fires whenever the task misses a deadline in any
RMS.periodic subscription. Use this to:

  - Log deadline violations for analysis
  - Trigger fallback behaviors
  - Reduce workload when overloaded
  - Alert operators in safety-critical systems

    subscriptions model =
        Sub.batch
            [ RMS.periodic 4 Frame
            , RMS.onDeadlineMiss DeadlineMissed
            ]

    update msg model =
        case msg of
            DeadlineMissed stats ->
                ( { model | missedDeadlines = model.missedDeadlines + 1 }
                , logDeadlineViolation stats
                )

-}
onDeadlineMiss : (PeriodStats -> msg) -> Sub msg
onDeadlineMiss toMsg =
    -- Kernel: tcelm_rms_deadline_miss_sub(callback)
    Elm.Kernel.RMS.onDeadlineMiss toMsg


-- PRIORITY ASSIGNMENT


{-| Assign RTEMS priority based on RMS policy.

In Rate Monotonic Scheduling, tasks with shorter periods get higher
priorities (lower RTEMS priority numbers). This function sets the
current task's priority according to its period.

    -- At task startup:
    RMS.assignPriority 4  -- 4ms period gets high priority

Priority mapping (RTEMS lower = higher priority):
  - 1ms period  -> priority 1 (highest)
  - 2ms period  -> priority 2
  - 4ms period  -> priority 4
  - 10ms period -> priority 10
  - etc.

-}
assignPriority : Int -> Task x ()
assignPriority periodMs =
    -- Kernel: tcelm_rms_assign_priority(period_ms)
    Elm.Kernel.RMS.assignPriority periodMs


{-| Calculate RTEMS priority from period.

Returns the priority value that should be assigned to a task
with the given period for proper RMS scheduling.

    priority = RMS.priorityFromPeriod 4  -- Returns 4

-}
priorityFromPeriod : Int -> Int
priorityFromPeriod periodMs =
    -- Simple mapping: priority = period in ms (capped at 255)
    min 255 (max 1 periodMs)
