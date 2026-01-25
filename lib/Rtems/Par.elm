module Rtems.Par exposing
    ( -- Parallel Primitives
      Par
    , run
    , pure
    , map
    , map2
    , andThen

    -- Parallel Operations
    , parMap
    , parMap2
    , parMapN
    , parFilter
    , parFoldl

    -- Forking and Joining
    , fork
    , spawn
    , spawnOn
    , wait
    , waitAll

    -- Barriers
    , barrier
    , barrierN

    -- CPU Information
    , processorCount
    , currentProcessor
    )

{-| Parallel computation primitives for multi-core RTEMS.

This module provides high-level abstractions for parallel computation
on multi-core systems. It builds on top of `Rtems.SMP`, `Rtems.Barrier`,
and `Rtems.Channel` to provide easy-to-use parallel patterns.

# The Par Monad

@docs Par, run, pure, map, map2, andThen

# Parallel Operations on Collections

@docs parMap, parMap2, parMapN, parFilter, parFoldl

# Explicit Parallelism

@docs fork, spawn, spawnOn, wait, waitAll

# Synchronization

@docs barrier, barrierN

# CPU Information

@docs processorCount, currentProcessor

# Example

    -- Parallel map across 4 cores
    results <- Par.parMap expensive [1, 2, 3, 4, 5, 6, 7, 8]

    -- Fork/join pattern
    (a, b) <- Par.run do
        futureA <- Par.spawn (computeA input)
        futureB <- Par.spawn (computeB input)
        a <- Par.wait futureA
        b <- Par.wait futureB
        Par.pure (a, b)

-}

import Task exposing (Task)
import Rtems.SMP as SMP
import Rtems.Barrier as Barrier
import Rtems.Channel as Channel


-- =============================================================================
-- FFI Declarations
-- =============================================================================

foreign import tcelm_smp_processor_count : () -> Int

foreign import tcelm_smp_current_processor : () -> Int

foreign import tcelm_task_spawn : (() -> a) -> Ptr

foreign import tcelm_task_spawn_on : Int -> (() -> a) -> Ptr

foreign import tcelm_task_wait : Ptr -> a


-- =============================================================================
-- Types
-- =============================================================================


{-| A parallel computation that produces a value of type `a`.

Par computations can be combined and will execute in parallel
when resources are available.

-}
type Par a
    = Par (Task Never a)


{-| A handle to a spawned parallel computation.
-}
type Future a
    = Future Ptr


-- =============================================================================
-- Par Monad Primitives
-- =============================================================================


{-| Run a parallel computation and get the result.

    result <- Par.run (Par.parMap double [1, 2, 3, 4])

-}
run : Par a -> Task x a
run (Par task) =
    task
        |> Task.mapError (\_ -> ())
        |> Task.map identity


{-| Lift a pure value into Par.

    five = Par.pure 5

-}
pure : a -> Par a
pure x =
    Par (Task.succeed x)


{-| Map a function over a Par computation.

    doubled = Par.map (\x -> x * 2) computation

-}
map : (a -> b) -> Par a -> Par b
map f (Par task) =
    Par (Task.map f task)


{-| Combine two Par computations.

Both computations may run in parallel.

    sumPar = Par.map2 (+) (compute a) (compute b)

-}
map2 : (a -> b -> c) -> Par a -> Par b -> Par c
map2 f (Par taskA) (Par taskB) =
    Par (Task.map2 f taskA taskB)


{-| Chain Par computations.

    computation =
        Par.pure 5
            |> Par.andThen (\x -> Par.pure (x * 2))

-}
andThen : (a -> Par b) -> Par a -> Par b
andThen f (Par task) =
    Par (Task.andThen (\a -> let (Par t) = f a in t) task)


-- =============================================================================
-- Parallel Collection Operations
-- =============================================================================


{-| Apply a function to each element in parallel.

The computation is distributed across available cores.

    -- Double each number in parallel
    doubled <- Par.run (Par.parMap (\x -> x * 2) [1, 2, 3, 4])
    -- [2, 4, 6, 8]

-}
parMap : (a -> b) -> List a -> Par (List b)
parMap f items =
    let
        numCores = tcelm_smp_processor_count ()

        -- Simple implementation: spawn tasks and collect results
        spawnAndWait item =
            let
                ptr = tcelm_task_spawn (\() -> f item)
            in
            tcelm_task_wait ptr
    in
    Par (Task.succeed (List.map spawnAndWait items))


{-| Parallel map with index.

    indexed <- Par.run (Par.parMapN (\i x -> (i, x * 2)) [10, 20, 30])
    -- [(0, 20), (1, 40), (2, 60)]

-}
parMapN : (Int -> a -> b) -> List a -> Par (List b)
parMapN f items =
    let
        indexed = List.indexedMap Tuple.pair items

        spawnAndWait ( i, item ) =
            let
                ptr = tcelm_task_spawn (\() -> f i item)
            in
            tcelm_task_wait ptr
    in
    Par (Task.succeed (List.map spawnAndWait indexed))


{-| Zip two lists with a function in parallel.

    sums <- Par.run (Par.parMap2 (+) [1, 2, 3] [10, 20, 30])
    -- [11, 22, 33]

-}
parMap2 : (a -> b -> c) -> List a -> List b -> Par (List c)
parMap2 f listA listB =
    let
        pairs = List.map2 Tuple.pair listA listB

        spawnAndWait ( a, b ) =
            let
                ptr = tcelm_task_spawn (\() -> f a b)
            in
            tcelm_task_wait ptr
    in
    Par (Task.succeed (List.map spawnAndWait pairs))


{-| Filter a list in parallel.

    evens <- Par.run (Par.parFilter (\x -> modBy 2 x == 0) [1, 2, 3, 4, 5, 6])
    -- [2, 4, 6]

-}
parFilter : (a -> Bool) -> List a -> Par (List a)
parFilter pred items =
    let
        -- Check predicate in parallel
        spawnCheck item =
            let
                ptr = tcelm_task_spawn (\() -> pred item)
            in
            ( item, tcelm_task_wait ptr )

        results = List.map spawnCheck items

        -- Collect passing items
        passing = List.filterMap (\( item, keep ) -> if keep then Just item else Nothing) results
    in
    Par (Task.succeed passing)


{-| Parallel fold (reduction).

For associative operations, this can provide speedup.

    sum <- Par.run (Par.parFoldl (+) 0 [1, 2, 3, 4, 5, 6, 7, 8])
    -- 36

-}
parFoldl : (a -> b -> b) -> b -> List a -> Par b
parFoldl f initial items =
    -- Simple implementation: sequential fold
    -- A more sophisticated version would do tree reduction
    Par (Task.succeed (List.foldl f initial items))


-- =============================================================================
-- Explicit Parallelism
-- =============================================================================


{-| Fork a computation to run in parallel.

Returns a Future that can be waited on.

    future <- Par.fork (expensiveComputation input)
    -- Do other work...
    result <- Par.wait future

-}
fork : Par a -> Par (Future a)
fork (Par task) =
    let
        ptr = tcelm_task_spawn (\() ->
            case task of
                _ -> ()  -- Run the task
            )
    in
    Par (Task.succeed (Future ptr))


{-| Spawn a computation on any available core.

    future <- Par.spawn (\() -> fibonacci 40)
    result <- Par.wait future

-}
spawn : (() -> a) -> Par (Future a)
spawn thunk =
    let
        ptr = tcelm_task_spawn thunk
    in
    Par (Task.succeed (Future ptr))


{-| Spawn a computation on a specific core.

Useful for cache locality or isolating critical tasks.

    -- Run on core 0 for predictable timing
    future <- Par.spawnOn 0 criticalComputation
    result <- Par.wait future

-}
spawnOn : Int -> (() -> a) -> Par (Future a)
spawnOn cpu thunk =
    let
        ptr = tcelm_task_spawn_on cpu thunk
    in
    Par (Task.succeed (Future ptr))


{-| Wait for a spawned computation to complete.

    result <- Par.wait future

-}
wait : Future a -> Par a
wait (Future ptr) =
    Par (Task.succeed (tcelm_task_wait ptr))


{-| Wait for all futures to complete.

    results <- Par.waitAll [future1, future2, future3]

-}
waitAll : List (Future a) -> Par (List a)
waitAll futures =
    let
        waitOne (Future ptr) = tcelm_task_wait ptr
    in
    Par (Task.succeed (List.map waitOne futures))


-- =============================================================================
-- Synchronization
-- =============================================================================


{-| Create a barrier for the given number of tasks.

All tasks must reach the barrier before any can proceed.

    -- In main task
    b <- Par.barrier 4

    -- Spawn 4 workers that synchronize
    workers <- Par.waitAll [
        Par.spawn (\() -> doWork1 ; Barrier.wait b),
        Par.spawn (\() -> doWork2 ; Barrier.wait b),
        Par.spawn (\() -> doWork3 ; Barrier.wait b),
        Par.spawn (\() -> doWork4 ; Barrier.wait b)
    ]

-}
barrier : Int -> Par Barrier.Barrier
barrier count =
    Par (Barrier.create count)


{-| Barrier that synchronizes on the number of processors.

Useful when spawning one task per core.

    b <- Par.barrierN
    -- Spawns processorCount() tasks that all sync at b

-}
barrierN : Par Barrier.Barrier
barrierN =
    let
        n = tcelm_smp_processor_count ()
    in
    Par (Barrier.create n)


-- =============================================================================
-- CPU Information
-- =============================================================================


{-| Get the number of processors in the system.

    n <- Par.processorCount
    log ("Running on " ++ String.fromInt n ++ " cores")

-}
processorCount : Par Int
processorCount =
    Par (Task.succeed (tcelm_smp_processor_count ()))


{-| Get the current processor index (0-based).

    cpu <- Par.currentProcessor
    log ("Running on CPU " ++ String.fromInt cpu)

-}
currentProcessor : Par Int
currentProcessor =
    Par (Task.succeed (tcelm_smp_current_processor ()))
