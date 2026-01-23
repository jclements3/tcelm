module Rtems.I2C exposing
    ( Bus
    , Device
    , open
    , close
    , Config
    , defaultConfig
    , Speed(..)
    , configure
    , write
    , read
    , writeRead
    , writeReg
    , readReg
    , readRegs
    , probe
    , scan
    )

{-| I2C (Inter-Integrated Circuit) for RTEMS.

I2C is a two-wire serial bus commonly used for sensors,
EEPROMs, RTCs, and other low-speed peripherals.

# Bus and Devices

@docs Bus, Device, open, close

# Configuration

@docs Config, defaultConfig, Speed, configure

# Basic Transfers

@docs write, read, writeRead

# Register Access

@docs writeReg, readReg, readRegs

# Device Discovery

@docs probe, scan

# Example

    -- Open I2C bus 1
    bus <- I2C.open 1

    -- Read temperature from sensor at address 0x48
    temp <- I2C.readReg bus 0x48 0x00
    let tempC = toFloat temp / 2.0

    -- Scan for devices
    devices <- I2C.scan bus
    -- Returns list of responding addresses

-}

import Task exposing (Task)


{-| An I2C bus handle.
-}
type Bus
    = Bus Int


{-| An I2C device (bus + address).
-}
type alias Device =
    { bus : Bus
    , address : Int -- 7-bit address
    }


{-| I2C bus speed.
-}
type Speed
    = Standard -- 100 kHz
    | Fast -- 400 kHz
    | FastPlus -- 1 MHz
    | HighSpeed -- 3.4 MHz


{-| I2C configuration.
-}
type alias Config =
    { speed : Speed
    , tenBitAddress : Bool -- Use 10-bit addressing
    , timeout : Int -- Timeout in milliseconds
    }


{-| Default configuration: Fast mode (400kHz), 7-bit addressing.
-}
defaultConfig : Config
defaultConfig =
    { speed = Fast
    , tenBitAddress = False
    , timeout = 1000
    }


{-| Open an I2C bus.

    bus <- I2C.open 1  -- Open I2C1

-}
open : Int -> Task x Bus
open busNum =
    -- Kernel: Open /dev/i2c-X or BSP-specific
    Elm.Kernel.I2C.open busNum
        |> Task.map (\_ -> Bus busNum)


{-| Close an I2C bus.
-}
close : Bus -> Task x ()
close (Bus busNum) =
    -- Kernel: Close I2C bus
    Elm.Kernel.I2C.close busNum


{-| Configure I2C bus.
-}
configure : Bus -> Config -> Task x ()
configure (Bus busNum) config =
    -- Kernel: ioctl to set I2C speed, etc.
    Elm.Kernel.I2C.configure busNum (speedToInt config.speed) config.timeout


speedToInt : Speed -> Int
speedToInt speed =
    case speed of
        Standard ->
            100000

        Fast ->
            400000

        FastPlus ->
            1000000

        HighSpeed ->
            3400000



-- =============================================================================
-- Basic Transfers
-- =============================================================================


{-| Write data to a device.

    -- Write 2 bytes to device at 0x50
    I2C.write bus 0x50 [0x00, 0xFF]

-}
write : Bus -> Int -> List Int -> Task x ()
write (Bus busNum) address data =
    -- Kernel: I2C write
    Elm.Kernel.I2C.write busNum address data


{-| Read data from a device.

    -- Read 4 bytes from device at 0x48
    data <- I2C.read bus 0x48 4

-}
read : Bus -> Int -> Int -> Task x (List Int)
read (Bus busNum) address count =
    -- Kernel: I2C read
    Elm.Kernel.I2C.read busNum address count


{-| Write then read (combined transaction).

This is a single I2C transaction with repeated start,
commonly used for register reads.

    -- Write register address, then read 2 bytes
    data <- I2C.writeRead bus 0x48 [0x00] 2

-}
writeRead : Bus -> Int -> List Int -> Int -> Task x (List Int)
writeRead (Bus busNum) address txData rxCount =
    -- Kernel: I2C combined write-read
    Elm.Kernel.I2C.writeRead busNum address txData rxCount



-- =============================================================================
-- Register Access (convenience functions)
-- =============================================================================


{-| Write to a single register.

    -- Write 0x42 to register 0x0A
    I2C.writeReg bus 0x68 0x0A 0x42

-}
writeReg : Bus -> Int -> Int -> Int -> Task x ()
writeReg bus address reg value =
    write bus address [ reg, value ]


{-| Read a single register.

    -- Read register 0x00
    value <- I2C.readReg bus 0x48 0x00

-}
readReg : Bus -> Int -> Int -> Task x Int
readReg bus address reg =
    writeRead bus address [ reg ] 1
        |> Task.map
            (\bytes ->
                case bytes of
                    b :: _ ->
                        b

                    [] ->
                        0
            )


{-| Read multiple consecutive registers.

    -- Read 6 registers starting at 0x3B (accelerometer data)
    data <- I2C.readRegs bus 0x68 0x3B 6

-}
readRegs : Bus -> Int -> Int -> Int -> Task x (List Int)
readRegs bus address startReg count =
    writeRead bus address [ startReg ] count



-- =============================================================================
-- Device Discovery
-- =============================================================================


{-| Check if a device responds at an address.

    exists <- I2C.probe bus 0x48
    if exists then
        initSensor bus
    else
        reportError "Sensor not found"

-}
probe : Bus -> Int -> Task x Bool
probe (Bus busNum) address =
    -- Kernel: Try to read 0 bytes, check for ACK
    Elm.Kernel.I2C.probe busNum address


{-| Scan the bus for all responding devices.

Returns a list of addresses that ACK'd.

    devices <- I2C.scan bus
    -- e.g., [0x48, 0x50, 0x68]

Note: Some devices may not respond to a scan,
and some addresses are reserved.

-}
scan : Bus -> Task x (List Int)
scan (Bus busNum) =
    -- Kernel: Probe addresses 0x08-0x77
    Elm.Kernel.I2C.scan busNum
