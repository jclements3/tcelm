module Rtems.Atomic exposing
    ( AtomicInt
    , AtomicBool

    -- AtomicInt operations
    , newInt
    , loadInt
    , storeInt
    , addInt
    , subInt
    , exchangeInt
    , compareExchangeInt

    -- AtomicBool operations
    , newBool
    , loadBool
    , storeBool
    , exchangeBool
    , compareExchangeBool

    -- Memory ordering
    , fence
    )

{-| Atomic operations for lock-free programming on RTEMS.

Atomic operations provide thread-safe access to shared variables
without using locks. They're useful for:

  - Counters and statistics
  - Flags and signals
  - Lock-free data structures
  - Memory barriers

# AtomicInt

@docs AtomicInt, newInt, loadInt, storeInt
@docs addInt, subInt, exchangeInt, compareExchangeInt

# AtomicBool

@docs AtomicBool, newBool, loadBool, storeBool
@docs exchangeBool, compareExchangeBool

# Memory Ordering

@docs fence

# Example

    -- Shared counter
    counter <- Atomic.newInt 0

    -- Task 1: increment
    old <- Atomic.addInt counter 1  -- Returns old value

    -- Task 2: read
    current <- Atomic.loadInt counter

    -- Atomic flag
    done <- Atomic.newBool False

    -- Task 1: signal completion
    Atomic.storeBool done True

    -- Task 2: check completion
    isDone <- Atomic.loadBool done

-}

import Task exposing (Task)


-- =============================================================================
-- FFI Declarations
-- =============================================================================

foreign import tcelm_atomic_int_new : Int -> Ptr

foreign import tcelm_atomic_int_load : Ptr -> Int

foreign import tcelm_atomic_int_store : Ptr -> Int -> ()

foreign import tcelm_atomic_int_add : Ptr -> Int -> Int

foreign import tcelm_atomic_int_sub : Ptr -> Int -> Int

foreign import tcelm_atomic_int_exchange : Ptr -> Int -> Int

foreign import tcelm_atomic_int_compare_exchange : Ptr -> Int -> Int -> Bool

foreign import tcelm_atomic_bool_new : Bool -> Ptr

foreign import tcelm_atomic_bool_load : Ptr -> Bool

foreign import tcelm_atomic_bool_store : Ptr -> Bool -> ()

foreign import tcelm_atomic_bool_exchange : Ptr -> Bool -> Bool

foreign import tcelm_atomic_bool_compare_exchange : Ptr -> Bool -> Bool -> Bool

foreign import tcelm_atomic_fence : () -> ()


-- =============================================================================
-- Types
-- =============================================================================


{-| An atomically accessible integer.
-}
type AtomicInt
    = AtomicInt Ptr


{-| An atomically accessible boolean.
-}
type AtomicBool
    = AtomicBool Ptr


-- =============================================================================
-- AtomicInt Operations
-- =============================================================================


{-| Create a new atomic integer with initial value.

    counter <- Atomic.newInt 0

-}
newInt : Int -> Task x AtomicInt
newInt initial =
    Task.succeed (AtomicInt (tcelm_atomic_int_new initial))


{-| Atomically load the current value.

    current <- Atomic.loadInt counter

-}
loadInt : AtomicInt -> Task x Int
loadInt (AtomicInt ptr) =
    Task.succeed (tcelm_atomic_int_load ptr)


{-| Atomically store a new value.

    Atomic.storeInt counter 42

-}
storeInt : AtomicInt -> Int -> Task x ()
storeInt (AtomicInt ptr) value =
    let
        _ = tcelm_atomic_int_store ptr value
    in
    Task.succeed ()


{-| Atomically add to the value and return the OLD value.

    -- counter was 5
    old <- Atomic.addInt counter 3  -- old = 5, counter is now 8

-}
addInt : AtomicInt -> Int -> Task x Int
addInt (AtomicInt ptr) delta =
    Task.succeed (tcelm_atomic_int_add ptr delta)


{-| Atomically subtract from the value and return the OLD value.

    -- counter was 10
    old <- Atomic.subInt counter 3  -- old = 10, counter is now 7

-}
subInt : AtomicInt -> Int -> Task x Int
subInt (AtomicInt ptr) delta =
    Task.succeed (tcelm_atomic_int_sub ptr delta)


{-| Atomically exchange the value and return the OLD value.

    -- counter was 5
    old <- Atomic.exchangeInt counter 42  -- old = 5, counter is now 42

-}
exchangeInt : AtomicInt -> Int -> Task x Int
exchangeInt (AtomicInt ptr) newValue =
    Task.succeed (tcelm_atomic_int_exchange ptr newValue)


{-| Atomic compare-and-swap.

If the current value equals `expected`, set it to `desired` and return True.
Otherwise, leave it unchanged and return False.

    -- Try to increment from 5 to 6
    success <- Atomic.compareExchangeInt counter 5 6
    -- success = True if counter was 5, now is 6
    -- success = False if counter wasn't 5, unchanged

This is the fundamental building block for lock-free algorithms.

-}
compareExchangeInt : AtomicInt -> Int -> Int -> Task x Bool
compareExchangeInt (AtomicInt ptr) expected desired =
    Task.succeed (tcelm_atomic_int_compare_exchange ptr expected desired)


-- =============================================================================
-- AtomicBool Operations
-- =============================================================================


{-| Create a new atomic boolean with initial value.

    flag <- Atomic.newBool False

-}
newBool : Bool -> Task x AtomicBool
newBool initial =
    Task.succeed (AtomicBool (tcelm_atomic_bool_new initial))


{-| Atomically load the current value.

    isDone <- Atomic.loadBool doneFlag

-}
loadBool : AtomicBool -> Task x Bool
loadBool (AtomicBool ptr) =
    Task.succeed (tcelm_atomic_bool_load ptr)


{-| Atomically store a new value.

    Atomic.storeBool doneFlag True

-}
storeBool : AtomicBool -> Bool -> Task x ()
storeBool (AtomicBool ptr) value =
    let
        _ = tcelm_atomic_bool_store ptr value
    in
    Task.succeed ()


{-| Atomically exchange the value and return the OLD value.

    -- Atomically set flag and check if it was already set
    wasSet <- Atomic.exchangeBool flag True
    if wasSet then
        log "Another task got here first"
    else
        log "We got here first"

-}
exchangeBool : AtomicBool -> Bool -> Task x Bool
exchangeBool (AtomicBool ptr) newValue =
    Task.succeed (tcelm_atomic_bool_exchange ptr newValue)


{-| Atomic compare-and-swap for booleans.

    -- Try to acquire a simple spinlock
    acquired <- Atomic.compareExchangeBool lock False True
    -- acquired = True if we got the lock

-}
compareExchangeBool : AtomicBool -> Bool -> Bool -> Task x Bool
compareExchangeBool (AtomicBool ptr) expected desired =
    Task.succeed (tcelm_atomic_bool_compare_exchange ptr expected desired)


-- =============================================================================
-- Memory Ordering
-- =============================================================================


{-| Full memory fence.

Ensures all memory operations before the fence are visible to all
processors before any operations after the fence.

    Atomic.storeInt dataReady 1
    Atomic.fence  -- Ensure dataReady is visible before proceeding

-}
fence : Task x ()
fence =
    let
        _ = tcelm_atomic_fence ()
    in
    Task.succeed ()
