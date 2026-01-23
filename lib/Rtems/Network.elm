module Rtems.Network exposing
    ( resolve
    , ping
    , isReachable
    , getLocalIp
    , NetworkInfo
    , getNetworkInfo
    )

{-| High-level Network utilities for RTEMS.

Provides DNS resolution and network diagnostics.

# DNS

@docs resolve

# Diagnostics

@docs ping, isReachable, getLocalIp, NetworkInfo, getNetworkInfo

-}

import Rtems.Socket as Socket exposing (Address)
import Task exposing (Task)


{-| Resolve a hostname to an IP address.

    ip <- Network.resolve "example.com"
    -- ip = "93.184.216.34"

-}
resolve : String -> Task Socket.Error String
resolve hostname =
    -- Kernel: gethostbyname
    Elm.Kernel.Network.resolve hostname


{-| Ping a host and measure round-trip time.

Returns the RTT in milliseconds, or fails if unreachable.

    rtt <- Network.ping "192.168.1.1"
    -- rtt = 1.5 (ms)

Note: ICMP ping requires elevated privileges on most systems.
Falls back to TCP connect on port 80 if ICMP unavailable.

-}
ping : String -> Task Socket.Error Float
ping host =
    -- Kernel: ICMP ping or TCP connect timing
    Elm.Kernel.Network.ping host


{-| Check if a host is reachable.

    reachable <- Network.isReachable "192.168.1.100" 8080
    if reachable then ...

-}
isReachable : String -> Int -> Task Socket.Error Bool
isReachable host port_ =
    -- Try to connect, immediately close
    Socket.tcpConnect (Socket.address host port_)
        |> Task.andThen
            (\sock ->
                Socket.close sock
                    |> Task.map (\_ -> True)
            )
        |> Task.onError (\_ -> Task.succeed False)


{-| Get the local IP address.

    localIp <- Network.getLocalIp
    -- localIp = "192.168.1.50"

-}
getLocalIp : Task Socket.Error String
getLocalIp =
    -- Kernel: Get local IP from interface
    Elm.Kernel.Network.getLocalIp


{-| Network interface information.
-}
type alias NetworkInfo =
    { interface : String -- e.g., "eth0"
    , ip : String -- IPv4 address
    , netmask : String -- Subnet mask
    , gateway : String -- Default gateway
    , mac : String -- MAC address
    , mtu : Int -- Maximum transmission unit
    , isUp : Bool -- Interface is up
    }


{-| Get network interface information.

    info <- Network.getNetworkInfo "eth0"

-}
getNetworkInfo : String -> Task Socket.Error NetworkInfo
getNetworkInfo interface =
    -- Kernel: ioctl for interface info
    Elm.Kernel.Network.getNetworkInfo interface
