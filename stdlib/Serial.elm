module Serial exposing
    ( Port
    , Config
    , BaudRate(..)
    , Parity(..)
    , StopBits(..)
    , open
    , close
    , write
    , read
    , readLine
    , flush
    , available
    , defaultConfig
    )

{-| Serial port I/O for UART communication.

On RTEMS, uses the termios API and RTEMS serial drivers.
Essential for sensor communication and debugging on embedded systems.

# Types
@docs Port, Config, BaudRate, Parity, StopBits

# Configuration
@docs defaultConfig

# Opening/Closing
@docs open, close

# Reading/Writing
@docs write, read, readLine

# Control
@docs flush, available

-}

import Task exposing (Task)


{-| A serial port handle.
On RTEMS, wraps a file descriptor for /dev/ttyS* or /dev/console.
-}
type Port
    = Port Int  -- file descriptor


{-| Baud rate options.
-}
type BaudRate
    = B9600
    | B19200
    | B38400
    | B57600
    | B115200
    | B230400
    | B460800
    | B921600


{-| Parity options.
-}
type Parity
    = NoParity
    | OddParity
    | EvenParity


{-| Stop bits options.
-}
type StopBits
    = OneStopBit
    | TwoStopBits


{-| Serial port configuration.
-}
type alias Config =
    { baudRate : BaudRate
    , parity : Parity
    , stopBits : StopBits
    , dataBits : Int  -- 5, 6, 7, or 8
    , timeout : Int   -- read timeout in deciseconds (0 = blocking)
    }


{-| Default configuration: 115200 8N1, blocking reads.
-}
defaultConfig : Config
defaultConfig =
    { baudRate = B115200
    , parity = NoParity
    , stopBits = OneStopBit
    , dataBits = 8
    , timeout = 0
    }


{-| Open a serial port.
On RTEMS, opens /dev/ttyS{n} and configures with termios.

    open 0 defaultConfig  -- Open /dev/ttyS0 with defaults

-}
open : Int -> Config -> Task String Port
open portNum config =
    -- Implemented by compiler as tcelm_serial_open
    open portNum config


{-| Close a serial port.

    close port

-}
close : Port -> Task x ()
close port =
    -- Implemented by compiler as tcelm_serial_close
    close port


{-| Write bytes to a serial port.
Returns number of bytes written.

    write port "AT+RST\r\n"

-}
write : Port -> String -> Task String Int
write port data =
    -- Implemented by compiler as tcelm_serial_write
    write port data


{-| Read up to N bytes from a serial port.
May return fewer bytes if timeout occurs.

    read port 256

-}
read : Port -> Int -> Task String String
read port maxBytes =
    -- Implemented by compiler as tcelm_serial_read
    read port maxBytes


{-| Read until newline or max bytes.

    readLine port 1024

-}
readLine : Port -> Int -> Task String String
readLine port maxBytes =
    -- Implemented by compiler as tcelm_serial_readline
    readLine port maxBytes


{-| Flush pending output.

    flush port

-}
flush : Port -> Task x ()
flush port =
    -- Implemented by compiler as tcelm_serial_flush
    flush port


{-| Get number of bytes available to read.

    available port

-}
available : Port -> Task x Int
available port =
    -- Implemented by compiler as tcelm_serial_available
    available port
