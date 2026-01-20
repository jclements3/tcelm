module Rtems.Gpio exposing
    ( Pin
    , pin
    , read
    , write
    , high
    , low
    , toggle
    , onRising
    , onFalling
    , onChange
    )

{-| GPIO (General Purpose Input/Output) for RTEMS.

This module provides type-safe GPIO operations.

# Pins

@docs Pin, pin

# Reading

@docs read

# Writing

@docs write, high, low, toggle

# Subscriptions

@docs onRising, onFalling, onChange

-}


{-| A GPIO pin identifier.
-}
type Pin
    = Pin Int


{-| Create a pin reference.

    led : Pin
    led =
        Gpio.pin 13

-}
pin : Int -> Pin
pin n =
    Pin n


{-| Read the current value of a pin.
Returns True for high, False for low.
-}
read : Pin -> Bool
read (Pin n) =
    -- Kernel function: tcelm_gpio_read
    Elm.Kernel.Rtems.gpioRead n


{-| Write a value to a pin.
-}
write : Pin -> Bool -> Cmd msg
write (Pin n) value =
    -- Kernel function: tcelm_gpio_write
    Elm.Kernel.Rtems.gpioWrite n value


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
toggle (Pin n) =
    -- Kernel function: tcelm_gpio_toggle
    Elm.Kernel.Rtems.gpioToggle n


{-| Subscribe to rising edge events on a pin.
-}
onRising : Pin -> msg -> Sub msg
onRising (Pin n) msg =
    -- Kernel function: tcelm_gpio_on_rising
    Elm.Kernel.Rtems.gpioOnRising n msg


{-| Subscribe to falling edge events on a pin.
-}
onFalling : Pin -> msg -> Sub msg
onFalling (Pin n) msg =
    -- Kernel function: tcelm_gpio_on_falling
    Elm.Kernel.Rtems.gpioOnFalling n msg


{-| Subscribe to any change on a pin.
-}
onChange : Pin -> (Bool -> msg) -> Sub msg
onChange (Pin n) toMsg =
    -- Kernel function: tcelm_gpio_on_change
    Elm.Kernel.Rtems.gpioOnChange n toMsg
