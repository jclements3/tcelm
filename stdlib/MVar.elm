module MVar exposing
    ( MVar
    , new
    , newEmpty
    , take
    , put
    , read
    , tryTake
    , tryPut
    , modify
    , swap
    )

{-| MVars provide shared mutable state with mutual exclusion.

On RTEMS, MVars use rtems_semaphore_* with priority inheritance
to prevent priority inversion in real-time systems.

An MVar can be thought of as a box that is either empty or contains a value.
- `take` removes the value (blocks if empty)
- `put` adds a value (blocks if full)
- `read` peeks at the value without removing it

# Types
@docs MVar

# Creating MVars
@docs new, newEmpty

# Operations
@docs take, put, read, tryTake, tryPut

# Combining Operations
@docs modify, swap

-}

import Task exposing (Task)


{-| An MVar is a synchronized mutable variable.
On RTEMS, this wraps an rtems_id for a binary semaphore
plus storage for the contained value.
-}
type MVar a
    = MVar Int  -- rtems_id


{-| Create a new MVar containing the given value.
On RTEMS, creates a binary semaphore with RTEMS_PRIORITY_INHERIT.

    new 0  -- MVar containing 0

-}
new : a -> Task x (MVar a)
new value =
    -- Implemented by compiler as tcelm_mvar_new
    new value


{-| Create a new empty MVar.
On RTEMS, creates a semaphore in the "taken" state.

    newEmpty  -- Empty MVar

-}
newEmpty : Task x (MVar a)
newEmpty =
    -- Implemented by compiler as tcelm_mvar_new_empty
    newEmpty


{-| Take the value from an MVar, leaving it empty.
On RTEMS, calls rtems_semaphore_obtain with RTEMS_WAIT.
Blocks if the MVar is empty.

    take mvar  -- Returns the value, MVar is now empty

-}
take : MVar a -> Task x a
take mvar =
    -- Implemented by compiler as tcelm_mvar_take
    take mvar


{-| Put a value into an MVar.
On RTEMS, calls rtems_semaphore_release.
Blocks if the MVar is full (another task must take first).

    put mvar 42

-}
put : MVar a -> a -> Task x ()
put mvar value =
    -- Implemented by compiler as tcelm_mvar_put
    put mvar value


{-| Read the value without removing it.
Equivalent to take followed by put.

    read mvar  -- Returns the value, MVar still contains it

-}
read : MVar a -> Task x a
read mvar =
    -- Implemented by compiler as tcelm_mvar_read
    read mvar


{-| Try to take without blocking.
Returns Nothing if MVar is empty.

    tryTake mvar

-}
tryTake : MVar a -> Task x (Maybe a)
tryTake mvar =
    -- Implemented by compiler as tcelm_mvar_try_take
    tryTake mvar


{-| Try to put without blocking.
Returns False if MVar is full.

    tryPut mvar 42

-}
tryPut : MVar a -> a -> Task x Bool
tryPut mvar value =
    -- Implemented by compiler as tcelm_mvar_try_put
    tryPut mvar value


{-| Atomically modify the contents of an MVar.
Takes the value, applies the function, puts the result back.

    modify mvar (\x -> (x + 1, x))  -- Increment and return old value

-}
modify : MVar a -> (a -> ( a, b )) -> Task x b
modify mvar fn =
    -- Implemented by compiler as tcelm_mvar_modify
    modify mvar fn


{-| Swap the contents of an MVar.
Atomically replaces the value and returns the old one.

    swap mvar newValue  -- Returns old value

-}
swap : MVar a -> a -> Task x a
swap mvar newValue =
    -- Implemented by compiler as tcelm_mvar_swap
    swap mvar newValue
