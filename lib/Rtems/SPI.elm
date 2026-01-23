module Rtems.SPI exposing
    ( Bus
    , Device
    , open
    , close
    , Config
    , defaultConfig
    , Mode(..)
    , BitOrder(..)
    , configure
    , transfer
    , write
    , read
    , writeRead
    , selectDevice
    , deselectDevice
    )

{-| SPI (Serial Peripheral Interface) for RTEMS.

SPI is a synchronous serial bus commonly used for sensors,
displays, SD cards, and other peripherals.

# Bus and Devices

@docs Bus, Device, open, close

# Configuration

@docs Config, defaultConfig, Mode, BitOrder, configure

# Data Transfer

@docs transfer, write, read, writeRead

# Chip Select

@docs selectDevice, deselectDevice

# Example

    -- Open SPI bus 0
    bus <- SPI.open 0

    -- Configure for 1MHz, Mode 0
    SPI.configure bus { SPI.defaultConfig | clockHz = 1000000 }

    -- Transfer data to device on CS pin 0
    response <- SPI.transfer bus 0 [0x9F, 0x00, 0x00, 0x00]

-}

import Task exposing (Task)


{-| An SPI bus handle.
-}
type Bus
    = Bus Int


{-| An SPI device (bus + chip select).
-}
type alias Device =
    { bus : Bus
    , csPin : Int
    }


{-| SPI clock mode (CPOL/CPHA).

  - Mode0: Clock idle low, sample on rising edge (most common)
  - Mode1: Clock idle low, sample on falling edge
  - Mode2: Clock idle high, sample on falling edge
  - Mode3: Clock idle high, sample on rising edge

-}
type Mode
    = Mode0
    | Mode1
    | Mode2
    | Mode3


{-| Bit transmission order.
-}
type BitOrder
    = MsbFirst
    | LsbFirst


{-| SPI configuration.
-}
type alias Config =
    { clockHz : Int -- Clock frequency in Hz
    , mode : Mode -- Clock polarity/phase
    , bitOrder : BitOrder -- MSB or LSB first
    , bitsPerWord : Int -- Usually 8
    , csActiveHigh : Bool -- CS polarity (usually False)
    }


{-| Default configuration: 1MHz, Mode 0, MSB first, 8 bits.
-}
defaultConfig : Config
defaultConfig =
    { clockHz = 1000000
    , mode = Mode0
    , bitOrder = MsbFirst
    , bitsPerWord = 8
    , csActiveHigh = False
    }


{-| Open an SPI bus.

    bus <- SPI.open 0  -- Open SPI0

-}
open : Int -> Task x Bus
open busNum =
    -- Kernel: Open /dev/spidevX.0 or BSP-specific
    Elm.Kernel.SPI.open busNum
        |> Task.map (\_ -> Bus busNum)


{-| Close an SPI bus.
-}
close : Bus -> Task x ()
close (Bus busNum) =
    -- Kernel: Close SPI bus
    Elm.Kernel.SPI.close busNum


{-| Configure SPI bus.
-}
configure : Bus -> Config -> Task x ()
configure (Bus busNum) config =
    -- Kernel: ioctl to set SPI mode, speed, etc.
    Elm.Kernel.SPI.configure busNum (configToInt config)


configToInt : Config -> Int
configToInt config =
    let
        modeInt =
            case config.mode of
                Mode0 ->
                    0

                Mode1 ->
                    1

                Mode2 ->
                    2

                Mode3 ->
                    3
    in
    modeInt


{-| Full-duplex transfer.

Simultaneously sends and receives data. The response has the
same length as the data sent.

    -- Read JEDEC ID from SPI flash
    response <- SPI.transfer bus 0 [0x9F, 0x00, 0x00, 0x00]
    -- response[1:3] contains the ID

-}
transfer : Bus -> Int -> List Int -> Task x (List Int)
transfer (Bus busNum) csPin txData =
    -- Kernel: SPI_IOC_MESSAGE or BSP transfer
    Elm.Kernel.SPI.transfer busNum csPin txData


{-| Write-only transfer.

Sends data without reading response.

    -- Write command to display
    SPI.write bus 0 [0x2C]  -- Write RAM command

-}
write : Bus -> Int -> List Int -> Task x ()
write (Bus busNum) csPin txData =
    -- Kernel: SPI write (ignore RX)
    Elm.Kernel.SPI.write busNum csPin txData


{-| Read-only transfer.

Reads data while sending zeros (or specified fill byte).

    -- Read 10 bytes from sensor
    data <- SPI.read bus 0 10

-}
read : Bus -> Int -> Int -> Task x (List Int)
read (Bus busNum) csPin count =
    -- Kernel: SPI read (TX zeros)
    Elm.Kernel.SPI.read busNum csPin count


{-| Write then read (half-duplex).

Useful for command-response protocols.

    -- Send command, then read 4 bytes of data
    response <- SPI.writeRead bus 0 [0x03, 0x00, 0x00, 0x00] 4

-}
writeRead : Bus -> Int -> List Int -> Int -> Task x (List Int)
writeRead (Bus busNum) csPin txData rxCount =
    -- Kernel: Write TX, then read RX
    Elm.Kernel.SPI.writeRead busNum csPin txData rxCount


{-| Manually assert chip select.

Useful for multi-transfer transactions.

    selectDevice bus 0
    transfer bus 0 [0x01]
    transfer bus 0 [0x02]
    deselectDevice bus 0

-}
selectDevice : Bus -> Int -> Task x ()
selectDevice (Bus busNum) csPin =
    -- Kernel: Assert CS
    Elm.Kernel.SPI.select busNum csPin


{-| Manually deassert chip select.
-}
deselectDevice : Bus -> Int -> Task x ()
deselectDevice (Bus busNum) csPin =
    -- Kernel: Deassert CS
    Elm.Kernel.SPI.deselect busNum csPin
