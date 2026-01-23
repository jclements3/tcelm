module Rtems.Gpio exposing
    ( Pin
    , pin
    , pinOnPort
    , open
    , close
    , Direction(..)
    , setDirection
    , read
    , write
    , high
    , low
    , toggle
    , onRising
    , onFalling
    , onChange
    , readMultiple
    , writeMultiple
    )

{-| GPIO (General Purpose Input/Output) for RTEMS.

This module provides type-safe GPIO operations.

# Pins

@docs Pin, pin, pinOnPort, open, close

# Configuration

@docs Direction, setDirection

# Reading

@docs read, readMultiple

# Writing

@docs write, high, low, toggle, writeMultiple

# Subscriptions

@docs onRising, onFalling, onChange

-}


{-| A GPIO pin identifier.
Combines port number and pin number.
-}
type Pin
    = Pin Int Int  -- Port, Pin


{-| GPIO pin direction.
-}
type Direction
    = Input
    | InputPullUp
    | InputPullDown
    | Output
    | OutputOpenDrain


{-| Create a pin reference (assumes port 0).

    led : Pin
    led =
        Gpio.pin 13

-}
pin : Int -> Pin
pin n =
    Pin 0 n


{-| Create a pin reference with explicit port.

    -- GPIO port 1, pin 5
    sensorPin : Pin
    sensorPin =
        Gpio.pinOnPort 1 5

-}
pinOnPort : Int -> Int -> Pin
pinOnPort port_ pinNum =
    Pin port_ pinNum


{-| Open a GPIO pin with direction.

    led <- Gpio.open (Gpio.pin 13) Output

-}
open : Pin -> Direction -> Cmd msg
open (Pin port_ pinNum) dir =
    -- Kernel function: tcelm_gpio_open
    Elm.Kernel.Rtems.gpioOpen port_ pinNum (directionToInt dir)


{-| Close a GPIO pin.
-}
close : Pin -> Cmd msg
close (Pin port_ pinNum) =
    -- Kernel function: tcelm_gpio_close
    Elm.Kernel.Rtems.gpioClose port_ pinNum


{-| Set pin direction.
-}
setDirection : Pin -> Direction -> Cmd msg
setDirection (Pin port_ pinNum) dir =
    -- Kernel function: tcelm_gpio_set_direction
    Elm.Kernel.Rtems.gpioSetDirection port_ pinNum (directionToInt dir)


directionToInt : Direction -> Int
directionToInt dir =
    case dir of
        Input -> 0
        Output -> 1
        InputPullUp -> 2
        InputPullDown -> 3
        OutputOpenDrain -> 4


{-| Read the current value of a pin.
Returns True for high, False for low.
-}
read : Pin -> Bool
read (Pin port_ pinNum) =
    -- Kernel function: tcelm_gpio_read
    Elm.Kernel.Rtems.gpioRead port_ pinNum


{-| Write a value to a pin.
-}
write : Pin -> Bool -> Cmd msg
write (Pin port_ pinNum) value =
    -- Kernel function: tcelm_gpio_write
    Elm.Kernel.Rtems.gpioWrite port_ pinNum value


{-| Set a pin high.
-}
high : Pin -> Cmd msg
high p =
    write p True


{-| Set a pin low.
-}
low : Pin -> Cmd msg
low p =
    write p False


{-| Toggle a pin.
-}
toggle : Pin -> Cmd msg
toggle (Pin port_ pinNum) =
    -- Kernel function: tcelm_gpio_toggle
    Elm.Kernel.Rtems.gpioToggle port_ pinNum


{-| Subscribe to rising edge events on a pin.
-}
onRising : Pin -> msg -> Sub msg
onRising (Pin port_ pinNum) msg =
    -- Kernel function: tcelm_gpio_on_rising
    Elm.Kernel.Rtems.gpioOnRising port_ pinNum msg


{-| Subscribe to falling edge events on a pin.
-}
onFalling : Pin -> msg -> Sub msg
onFalling (Pin port_ pinNum) msg =
    -- Kernel function: tcelm_gpio_on_falling
    Elm.Kernel.Rtems.gpioOnFalling port_ pinNum msg


{-| Subscribe to any change on a pin.
-}
onChange : Pin -> (Bool -> msg) -> Sub msg
onChange (Pin port_ pinNum) toMsg =
    -- Kernel function: tcelm_gpio_on_change
    Elm.Kernel.Rtems.gpioOnChange port_ pinNum toMsg


{-| Read multiple pins as an integer value.
Bit 0 = first pin, bit 1 = second pin, etc.
-}
readMultiple : List Pin -> Int
readMultiple pins =
    -- Kernel function: tcelm_gpio_read_multi
    Elm.Kernel.Rtems.gpioReadMultiple pins


{-| Write to multiple pins from an integer value.
Bit 0 = first pin, bit 1 = second pin, etc.
-}
writeMultiple : List Pin -> Int -> Cmd msg
writeMultiple pins value =
    -- Kernel function: tcelm_gpio_write_multi
    Elm.Kernel.Rtems.gpioWriteMultiple pins value
