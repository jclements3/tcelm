module IO exposing
    ( IO
    , pure
    , map
    , andThen
    , bind
    , perform
    , print
    , println
    , readLine
    , getArgs
    , exit
    )

{-| IO represents effectful computations.

The IO monad sequences side effects in a pure functional way.
On RTEMS, IO operations use the RTEMS I/O manager.

# Types
@docs IO

# Creating IO
@docs pure

# Transforming IO
@docs map, andThen, bind

# Running IO
@docs perform

# Console I/O
@docs print, println, readLine

# System
@docs getArgs, exit

-}


{-| An IO action that produces a value of type a.
-}
type IO a
    = IO (() -> a)


{-| Wrap a pure value in IO.

    pure 42  -- IO Int that produces 42

-}
pure : a -> IO a
pure value =
    -- Implemented by compiler
    pure value


{-| Transform the result of an IO action.

    map String.length (readLine)  -- IO Int

-}
map : (a -> b) -> IO a -> IO b
map fn io =
    -- Implemented by compiler
    map fn io


{-| Chain IO actions together.

    readLine
        |> andThen (\name -> println ("Hello, " ++ name))

-}
andThen : (a -> IO b) -> IO a -> IO b
andThen fn io =
    -- Implemented by compiler
    andThen fn io


{-| Alias for andThen with arguments flipped (Haskell's >>=).

    readLine |> bind (\name -> println ("Hello, " ++ name))

-}
bind : IO a -> (a -> IO b) -> IO b
bind io fn =
    andThen fn io


{-| Execute an IO action and return the result.
This is the "escape hatch" from pure code.
Only call this from main or other IO contexts.

    perform (println "Hello")

-}
perform : IO a -> a
perform io =
    -- Implemented by compiler as tcelm_io_perform
    perform io


{-| Print a string to stdout (no newline).
On RTEMS, uses the console driver.

    print "Enter name: "

-}
print : String -> IO ()
print str =
    -- Implemented by compiler as tcelm_io_print
    print str


{-| Print a string to stdout with newline.

    println "Hello, World!"

-}
println : String -> IO ()
println str =
    -- Implemented by compiler as tcelm_io_println
    println str


{-| Read a line from stdin.
On RTEMS, reads from the console driver.

    readLine  -- IO String

-}
readLine : IO String
readLine =
    -- Implemented by compiler as tcelm_io_readline
    readLine


{-| Get command line arguments.

    getArgs  -- IO (List String)

-}
getArgs : IO (List String)
getArgs =
    -- Implemented by compiler as tcelm_io_getargs
    getArgs


{-| Exit the program with the given status code.
On RTEMS, calls rtems_shutdown_executive.

    exit 0  -- Success
    exit 1  -- Failure

-}
exit : Int -> IO ()
exit code =
    -- Implemented by compiler as tcelm_io_exit
    exit code
