module Rtems.Uart exposing
    ( Port
    , port_
    , console
    , open
    , close
    , write
    , writeLine
    , print
    , read
    , readLine
    , readNonBlock
    , available
    , onReceive
    , onLine
    , Config
    , defaultConfig
    , Parity(..)
    , StopBits(..)
    , FlowControl(..)
    , configure
    , flush
    , Stats
    , getStats
    )

{-| UART (Serial) communication for RTEMS.

# Ports

@docs Port, port_, console, open, close

# Configuration

@docs Config, defaultConfig, Parity, StopBits, FlowControl, configure

# Writing

@docs write, writeLine, print, flush

# Reading

@docs read, readLine, readNonBlock, available

# Subscriptions

@docs onReceive, onLine

# Statistics

@docs Stats, getStats

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


{-| Subscribe to line events (called when newline received).
-}
onLine : Port -> (String -> msg) -> Sub msg
onLine (Port n) toMsg =
    -- Kernel function: tcelm_uart_on_line
    Elm.Kernel.Rtems.uartOnLine n toMsg



-- =============================================================================
-- Configuration
-- =============================================================================


{-| UART parity modes.
-}
type Parity
    = NoParity
    | OddParity
    | EvenParity


{-| UART stop bits.
-}
type StopBits
    = OneStopBit
    | TwoStopBits


{-| UART flow control.
-}
type FlowControl
    = NoFlowControl
    | Hardware -- RTS/CTS
    | Software -- XON/XOFF


{-| UART configuration.
-}
type alias Config =
    { baudRate : Int
    , dataBits : Int
    , parity : Parity
    , stopBits : StopBits
    , flowControl : FlowControl
    , rxBufferSize : Int
    , txBufferSize : Int
    , timeoutMs : Int
    }


{-| Default configuration: 115200 8N1
-}
defaultConfig : Config
defaultConfig =
    { baudRate = 115200
    , dataBits = 8
    , parity = NoParity
    , stopBits = OneStopBit
    , flowControl = NoFlowControl
    , rxBufferSize = 256
    , txBufferSize = 256
    , timeoutMs = 0
    }


{-| Open a UART port with configuration.
-}
open : Int -> Config -> Port
open portNum config =
    -- Kernel function: tcelm_uart_open
    Port portNum


{-| Close a UART port.
-}
close : Port -> Cmd msg
close (Port n) =
    -- Kernel function: tcelm_uart_close
    Elm.Kernel.Rtems.uartClose n


{-| Configure an open UART port.
-}
configure : Port -> Config -> Cmd msg
configure (Port n) config =
    -- Kernel function: tcelm_uart_configure
    Elm.Kernel.Rtems.uartConfigure n config


{-| Flush transmit buffer (wait for pending data to be sent).
-}
flush : Port -> Cmd msg
flush (Port n) =
    -- Kernel function: tcelm_uart_flush
    Elm.Kernel.Rtems.uartFlush n


{-| Read available data without blocking.
Returns empty string if no data available.
-}
readNonBlock : Port -> Int -> String
readNonBlock (Port n) maxBytes =
    -- Kernel function: tcelm_uart_read_nonblock
    Elm.Kernel.Rtems.uartReadNonBlock n maxBytes


{-| Check how many bytes are available to read.
-}
available : Port -> Int
available (Port n) =
    -- Kernel function: tcelm_uart_available
    Elm.Kernel.Rtems.uartAvailable n



-- =============================================================================
-- Statistics
-- =============================================================================


{-| UART port statistics.
-}
type alias Stats =
    { bytesSent : Int
    , bytesReceived : Int
    , rxOverruns : Int
    , framingErrors : Int
    , parityErrors : Int
    }


{-| Get port statistics.
-}
getStats : Port -> Stats
getStats (Port n) =
    -- Kernel function: tcelm_uart_get_stats
    Elm.Kernel.Rtems.uartGetStats n
