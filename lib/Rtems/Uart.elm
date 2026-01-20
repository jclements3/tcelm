module Rtems.Uart exposing
    ( Port
    , port_
    , console
    , write
    , writeLine
    , print
    , read
    , readLine
    , onReceive
    )

{-| UART (Serial) communication for RTEMS.

# Ports

@docs Port, port_, console

# Writing

@docs write, writeLine, print

# Reading

@docs read, readLine

# Subscriptions

@docs onReceive

-}


{-| A UART port identifier.
-}
type Port
    = Port Int


{-| Create a port reference.

    serial1 : Port
    serial1 =
        Uart.port_ 1

-}
port_ : Int -> Port
port_ n =
    Port n


{-| The default console port (usually UART0 or debug output).
-}
console : Port
console =
    Port 0


{-| Write a string to a UART port.
-}
write : Port -> String -> Cmd msg
write (Port n) str =
    -- Kernel function: tcelm_uart_write
    Elm.Kernel.Rtems.uartWrite n str


{-| Write a string followed by a newline.
-}
writeLine : Port -> String -> Cmd msg
writeLine p str =
    write p (str ++ "\n")


{-| Print to console (convenience function).
-}
print : String -> Cmd msg
print str =
    write console str


{-| Read up to n bytes from a UART port.
Returns immediately with available data (non-blocking).
-}
read : Port -> Int -> String
read (Port n) count =
    -- Kernel function: tcelm_uart_read
    Elm.Kernel.Rtems.uartRead n count


{-| Read a line from a UART port (blocking until newline).
-}
readLine : Port -> String
readLine (Port n) =
    -- Kernel function: tcelm_uart_read_line
    Elm.Kernel.Rtems.uartReadLine n


{-| Subscribe to receive events on a UART port.
The message is sent whenever data is available.
-}
onReceive : Port -> (String -> msg) -> Sub msg
onReceive (Port n) toMsg =
    -- Kernel function: tcelm_uart_on_receive
    Elm.Kernel.Rtems.uartOnReceive n toMsg
