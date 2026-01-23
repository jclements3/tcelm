module Rtems.Sync exposing
    ( -- Semaphores
      Semaphore
    , createSemaphore
    , createSemaphoreWithMax
    , acquire
    , acquireTimeout
    , tryAcquire
    , release
    , releaseMultiple
    , getCount
    , flush
    , deleteSemaphore

    -- Mutexes
    , Mutex
    , createMutex
    , lock
    , lockTimeout
    , tryLock
    , unlock
    , deleteMutex

    -- Configuration
    , SemaphoreConfig
    , defaultSemaphoreConfig
    )

{-| Synchronization primitives for RTEMS.

This module provides Elm bindings to RTEMS semaphores and mutexes,
enabling safe shared resource access between tasks.

# Semaphores

Counting semaphores for resource counting and signaling.

@docs Semaphore, createSemaphore, createSemaphoreWithMax
@docs acquire, acquireTimeout, tryAcquire
@docs release, releaseMultiple, getCount, flush, deleteSemaphore

# Mutexes

Mutual exclusion locks for protecting critical sections.
Mutexes are binary semaphores with ownership semantics.

@docs Mutex, createMutex, lock, lockTimeout, tryLock, unlock, deleteMutex

# Configuration

@docs SemaphoreConfig, defaultSemaphoreConfig

-}

import Task exposing (Task)


-- =============================================================================
-- Semaphores
-- =============================================================================


{-| A counting semaphore handle.

Semaphores maintain an internal count:

  - `acquire` decrements the count (blocks if count is 0)
  - `release` increments the count (wakes a waiting task)

-}
type Semaphore
    = Semaphore


{-| Configuration for creating a semaphore.
-}
type alias SemaphoreConfig =
    { initialCount : Int
    , maxCount : Int -- 0 = unlimited
    , priorityCeiling : Bool
    , ceilingPriority : Int
    , name : String
    }


{-| Default semaphore configuration.
-}
defaultSemaphoreConfig : SemaphoreConfig
defaultSemaphoreConfig =
    { initialCount = 0
    , maxCount = 0
    , priorityCeiling = False
    , ceilingPriority = 0
    , name = ""
    }


{-| Create a counting semaphore with initial count.

    -- Create a semaphore with 3 permits
    createSemaphore 3

-}
createSemaphore : Int -> Task x Semaphore
createSemaphore initialCount =
    -- Kernel: tcelm_semaphore_create_with_count
    Elm.Kernel.Sync.createSemaphore initialCount


{-| Create a semaphore with initial and max count.

    -- Create a bounded semaphore (max 5 permits)
    createSemaphoreWithMax 0 5

-}
createSemaphoreWithMax : Int -> Int -> Task x Semaphore
createSemaphoreWithMax initialCount maxCount =
    -- Kernel: tcelm_semaphore_create
    Elm.Kernel.Sync.createSemaphoreWithMax initialCount maxCount


{-| Acquire the semaphore (blocking).

Decrements the count. If count is 0, blocks until another task
calls `release`.

    acquire sem
        |> Task.andThen (\_ -> doWork)
        |> Task.andThen (\_ -> release sem)

-}
acquire : Semaphore -> Task x ()
acquire sem =
    -- Kernel: tcelm_semaphore_acquire
    Elm.Kernel.Sync.acquire sem


{-| Acquire with timeout in milliseconds.

Returns `Err Timeout` if the semaphore cannot be acquired within
the specified time.

    case acquireTimeout 1000 sem of
        Ok () -> doWork
        Err Timeout -> handleTimeout

-}
acquireTimeout : Int -> Semaphore -> Task AcquireError ()
acquireTimeout timeoutMs sem =
    -- Kernel: tcelm_semaphore_acquire_timeout
    Elm.Kernel.Sync.acquireTimeout timeoutMs sem


{-| Error types for acquire operations.
-}
type AcquireError
    = Timeout
    | SemaphoreDeleted


{-| Try to acquire without blocking.

Returns immediately with `Ok ()` if acquired, `Err WouldBlock` otherwise.

    if tryAcquire sem == Ok () then
        doWork
    else
        doSomethingElse

-}
tryAcquire : Semaphore -> Task TryAcquireError ()
tryAcquire sem =
    -- Kernel: tcelm_semaphore_try_acquire
    Elm.Kernel.Sync.tryAcquire sem


{-| Error when try-acquire fails.
-}
type TryAcquireError
    = WouldBlock


{-| Release the semaphore.

Increments the count and wakes one waiting task (if any).

-}
release : Semaphore -> Task x ()
release sem =
    -- Kernel: tcelm_semaphore_release
    Elm.Kernel.Sync.release sem


{-| Release multiple permits at once.

    -- Release 3 permits
    releaseMultiple 3 sem

-}
releaseMultiple : Int -> Semaphore -> Task x ()
releaseMultiple count sem =
    -- Kernel: tcelm_semaphore_release_multiple
    Elm.Kernel.Sync.releaseMultiple count sem


{-| Get the current semaphore count.

Note: The count may change immediately after this call returns
if other tasks are accessing the semaphore.

-}
getCount : Semaphore -> Task x Int
getCount sem =
    -- Kernel: tcelm_semaphore_get_count
    Elm.Kernel.Sync.getCount sem


{-| Flush all waiting tasks.

Wakes all tasks waiting on the semaphore. They will return with
an error indicating the semaphore was flushed.

Returns the number of tasks that were waiting.

-}
flush : Semaphore -> Task x Int
flush sem =
    -- Kernel: tcelm_semaphore_flush
    Elm.Kernel.Sync.flush sem


{-| Delete a semaphore.

All waiting tasks will be released with an error.

-}
deleteSemaphore : Semaphore -> Task x ()
deleteSemaphore sem =
    -- Kernel: tcelm_semaphore_delete
    Elm.Kernel.Sync.deleteSemaphore sem



-- =============================================================================
-- Mutexes
-- =============================================================================


{-| A mutual exclusion lock.

Mutexes are binary semaphores with ownership semantics:

  - Only the task that acquired the mutex can release it
  - Supports priority inheritance to prevent priority inversion
  - Recursive locking is NOT supported

-}
type Mutex
    = Mutex


{-| Create a new mutex.

    mutex <- createMutex
    lock mutex
    -- critical section
    unlock mutex

-}
createMutex : Task x Mutex
createMutex =
    -- Kernel: tcelm_mutex_create
    Elm.Kernel.Sync.createMutex ()


{-| Acquire the mutex (blocking).

Blocks until the mutex is available, then acquires it.
The calling task becomes the owner.

-}
lock : Mutex -> Task x ()
lock mutex =
    -- Kernel: tcelm_mutex_lock
    Elm.Kernel.Sync.lock mutex


{-| Acquire with timeout in milliseconds.

    case lockTimeout 100 mutex of
        Ok () ->
            -- Got the lock
            unlock mutex

        Err Timeout ->
            -- Lock not available within 100ms
            handleContention

-}
lockTimeout : Int -> Mutex -> Task AcquireError ()
lockTimeout timeoutMs mutex =
    -- Kernel: tcelm_mutex_lock_timeout
    Elm.Kernel.Sync.lockTimeout timeoutMs mutex


{-| Try to acquire without blocking.

Returns immediately with `Ok ()` if acquired, `Err WouldBlock` if
the mutex is held by another task.

-}
tryLock : Mutex -> Task TryAcquireError ()
tryLock mutex =
    -- Kernel: tcelm_mutex_try_lock
    Elm.Kernel.Sync.tryLock mutex


{-| Release the mutex.

Must be called by the task that holds the lock.

-}
unlock : Mutex -> Task x ()
unlock mutex =
    -- Kernel: tcelm_mutex_unlock
    Elm.Kernel.Sync.unlock mutex


{-| Delete a mutex.

The mutex must not be held when deleted.

-}
deleteMutex : Mutex -> Task x ()
deleteMutex mutex =
    -- Kernel: tcelm_mutex_delete
    Elm.Kernel.Sync.deleteMutex mutex
