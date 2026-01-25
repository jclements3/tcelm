module Rtems.MVar exposing
    ( MVar
    , new
    , newEmpty
    , take
    , takeTimeout
    , tryTake
    , put
    , putTimeout
    , tryPut
    , read
    , swap
    , modify
    , isEmpty
    , delete
    )

{-| MVars - synchronized mutable variables for RTEMS.

An MVar is a mutable location that is either empty or contains a value.
It provides mutual exclusion with priority inheritance to prevent
priority inversion.

# Semantics

  - `take` removes the value, leaving the MVar empty (blocks if empty)
  - `put` fills the MVar with a value (blocks if full)
  - `read` returns the value without removing it

This provides a natural way to implement:

  - Synchronized shared state
  - One-place channels
  - Binary semaphores
  - Lock variables

# Creating MVars

@docs MVar, new, newEmpty

# Taking Values

@docs take, takeTimeout, tryTake

# Putting Values

@docs put, putTimeout, tryPut

# Reading and Modifying

@docs read, swap, modify

# Utilities

@docs isEmpty, delete

# Example

    -- Shared counter between tasks
    counter <- MVar.new 0

    -- Task 1: increment
    n <- MVar.take counter
    MVar.put counter (n + 1)

    -- Task 2: read current value
    current <- MVar.read counter
    log ("Counter is " ++ String.fromInt current)

-}

import Task exposing (Task)


-- =============================================================================
-- FFI Declarations
-- =============================================================================

foreign import tcelm_mvar_new : a -> Ptr

foreign import tcelm_mvar_new_empty : () -> Ptr

foreign import tcelm_mvar_take : Ptr -> a

foreign import tcelm_mvar_take_timeout : Ptr -> Int -> a

foreign import tcelm_mvar_try_take : Ptr -> a

foreign import tcelm_mvar_put : Ptr -> a -> Int

foreign import tcelm_mvar_put_timeout : Ptr -> a -> Int -> Int

foreign import tcelm_mvar_try_put : Ptr -> a -> Int

foreign import tcelm_mvar_read : Ptr -> a

foreign import tcelm_mvar_swap : Ptr -> a -> a

foreign import tcelm_mvar_is_empty : Ptr -> Bool

foreign import tcelm_mvar_delete : Ptr -> ()


-- =============================================================================
-- Types
-- =============================================================================


{-| A synchronized mutable variable.

An MVar is either empty or contains exactly one value.
Operations on MVars are atomic and use priority inheritance.

-}
type MVar a
    = MVar Ptr


-- =============================================================================
-- Creating MVars
-- =============================================================================


{-| Create an MVar containing the given value.

    counter <- MVar.new 0
    config <- MVar.new { timeout = 1000, retries = 3 }

-}
new : a -> Task x (MVar a)
new value =
    Task.succeed (MVar (tcelm_mvar_new value))


{-| Create an empty MVar.

    slot <- MVar.newEmpty
    -- Some task will put a value later
    result <- MVar.take slot  -- Blocks until value is put

-}
newEmpty : Task x (MVar a)
newEmpty =
    Task.succeed (MVar (tcelm_mvar_new_empty ()))


-- =============================================================================
-- Taking Values
-- =============================================================================


{-| Take the value from an MVar, leaving it empty.

Blocks if the MVar is already empty until another task puts a value.

    value <- MVar.take mvar
    -- MVar is now empty

-}
take : MVar a -> Task x a
take (MVar ptr) =
    Task.succeed (tcelm_mvar_take ptr)


{-| Take with timeout in milliseconds.

Returns `Nothing` if the MVar remains empty for the duration.

    case MVar.takeTimeout 1000 mvar of
        Just value -> process value
        Nothing -> log "MVar empty for 1 second"

-}
takeTimeout : Int -> MVar a -> Task x (Maybe a)
takeTimeout timeoutMs (MVar ptr) =
    Task.succeed (tcelm_mvar_take_timeout ptr timeoutMs)


{-| Try to take without blocking.

Returns `Nothing` if the MVar is empty.

    case MVar.tryTake mvar of
        Just value -> process value
        Nothing -> log "MVar is empty"

-}
tryTake : MVar a -> Task x (Maybe a)
tryTake (MVar ptr) =
    Task.succeed (tcelm_mvar_try_take ptr)


-- =============================================================================
-- Putting Values
-- =============================================================================


{-| Put a value into an MVar.

Blocks if the MVar is already full until another task takes the value.

    MVar.put mvar newValue
    -- MVar now contains newValue

-}
put : MVar a -> a -> Task x ()
put (MVar ptr) value =
    let
        result = tcelm_mvar_put ptr value
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail ()


{-| Put with timeout in milliseconds.

Returns `Err ()` if the MVar remains full for the duration.

    case MVar.putTimeout 1000 mvar value of
        Ok () -> log "Put succeeded"
        Err () -> log "MVar full for 1 second"

-}
putTimeout : Int -> MVar a -> a -> Task () ()
putTimeout timeoutMs (MVar ptr) value =
    let
        result = tcelm_mvar_put_timeout ptr value timeoutMs
    in
    if result == 0 then
        Task.succeed ()
    else
        Task.fail ()


{-| Try to put without blocking.

Returns `False` if the MVar is full.

    if MVar.tryPut mvar value then
        log "Put succeeded"
    else
        log "MVar is full"

-}
tryPut : MVar a -> a -> Task x Bool
tryPut (MVar ptr) value =
    let
        result = tcelm_mvar_try_put ptr value
    in
    Task.succeed (result == 0)


-- =============================================================================
-- Reading and Modifying
-- =============================================================================


{-| Read the value without removing it.

Equivalent to take followed by put. Blocks if empty.

    current <- MVar.read mvar
    -- MVar still contains the value

-}
read : MVar a -> Task x a
read (MVar ptr) =
    Task.succeed (tcelm_mvar_read ptr)


{-| Atomically swap the contents of an MVar.

Replaces the current value with a new one and returns the old value.
Blocks if the MVar is empty.

    old <- MVar.swap mvar newValue
    -- MVar now contains newValue, we have old

-}
swap : MVar a -> a -> Task x a
swap (MVar ptr) newValue =
    Task.succeed (tcelm_mvar_swap ptr newValue)


{-| Atomically modify the contents of an MVar.

Takes the value, applies a function, and puts the result back.
The function returns `(newValue, result)` - the MVar gets `newValue`
and `modify` returns `result`.

    -- Increment counter and get new value
    newCount <- MVar.modify counter (\n -> (n + 1, n + 1))

    -- Pop from a stack
    top <- MVar.modify stack (\(h :: t) -> (t, h))

-}
modify : MVar a -> (a -> ( a, b )) -> Task x b
modify (MVar ptr) f =
    let
        oldValue = tcelm_mvar_take ptr
        ( newValue, result ) = f oldValue
        _ = tcelm_mvar_put ptr newValue
    in
    Task.succeed result


-- =============================================================================
-- Utilities
-- =============================================================================


{-| Check if an MVar is empty (non-blocking).

Note: The value may change immediately after this call returns.

-}
isEmpty : MVar a -> Task x Bool
isEmpty (MVar ptr) =
    Task.succeed (tcelm_mvar_is_empty ptr)


{-| Delete an MVar.

The MVar must be empty when deleted. Any waiting tasks will be released.

-}
delete : MVar a -> Task x ()
delete (MVar ptr) =
    let
        _ = tcelm_mvar_delete ptr
    in
    Task.succeed ()
