module Rtems.Socket exposing
    ( Socket
    , Address
    , address
    , localhost
    , any
    , Error(..)
    , tcpConnect
    , tcpListen
    , tcpAccept
    , udpOpen
    , udpBind
    , send
    , sendTo
    , receive
    , receiveFrom
    , close
    , shutdown
    , ShutdownMode(..)
    , setOption
    , Option(..)
    , onConnection
    , onData
    )

{-| TCP/UDP Socket API for RTEMS.

Provides BSD-style socket operations wrapped in Elm's Task abstraction
for safe, non-blocking network I/O.

# Sockets and Addresses

@docs Socket, Address, address, localhost, any

# Errors

@docs Error

# TCP Operations

@docs tcpConnect, tcpListen, tcpAccept

# UDP Operations

@docs udpOpen, udpBind

# Data Transfer

@docs send, sendTo, receive, receiveFrom

# Socket Control

@docs close, shutdown, ShutdownMode, setOption, Option

# Subscriptions

@docs onConnection, onData

# Example - TCP Client

    -- Connect to server
    sock <- Socket.tcpConnect (Socket.address "192.168.1.100" 8080)

    -- Send request
    Socket.send sock "GET / HTTP/1.0\r\n\r\n"

    -- Receive response
    response <- Socket.receive sock 4096

    -- Close connection
    Socket.close sock

# Example - TCP Server

    -- Listen on port 8080
    server <- Socket.tcpListen Socket.any 8080

    -- In subscriptions
    subscriptions model =
        Socket.onConnection server ClientConnected

    -- Handle new client
    update msg model =
        case msg of
            ClientConnected clientSock ->
                -- Handle client...

-}

import Task exposing (Task)


{-| A socket handle.
-}
type Socket
    = Socket Int


{-| A network address (IP + port).
-}
type Address
    = Address String Int


{-| Create an address from IP string and port.

    addr = Socket.address "192.168.1.100" 8080

-}
address : String -> Int -> Address
address ip port_ =
    Address ip port_


{-| Localhost address (127.0.0.1).

    addr = Socket.localhost 8080

-}
localhost : Int -> Address
localhost port_ =
    Address "127.0.0.1" port_


{-| Any interface (0.0.0.0) - for servers.

    addr = Socket.any 8080

-}
any : Int -> Address
any port_ =
    Address "0.0.0.0" port_


{-| Socket errors.
-}
type Error
    = ConnectionRefused
    | ConnectionReset
    | Timeout
    | AddressInUse
    | AddressNotAvailable
    | NetworkUnreachable
    | HostUnreachable
    | WouldBlock
    | InvalidArgument
    | Unknown Int



-- =============================================================================
-- TCP Operations
-- =============================================================================


{-| Connect to a TCP server.

    sock <- Socket.tcpConnect (Socket.address "192.168.1.100" 8080)

-}
tcpConnect : Address -> Task Error Socket
tcpConnect (Address ip port_) =
    -- Kernel: Create socket, connect
    Elm.Kernel.Socket.tcpConnect ip port_
        |> Task.map Socket


{-| Create a TCP listening socket.

    server <- Socket.tcpListen Socket.any 8080

-}
tcpListen : Address -> Int -> Task Error Socket
tcpListen (Address ip port_) backlog =
    -- Kernel: Create socket, bind, listen
    Elm.Kernel.Socket.tcpListen ip port_ backlog
        |> Task.map Socket


{-| Accept an incoming TCP connection.

Blocks until a client connects.

    client <- Socket.tcpAccept server

-}
tcpAccept : Socket -> Task Error Socket
tcpAccept (Socket fd) =
    -- Kernel: Accept connection
    Elm.Kernel.Socket.tcpAccept fd
        |> Task.map Socket



-- =============================================================================
-- UDP Operations
-- =============================================================================


{-| Open a UDP socket.

    sock <- Socket.udpOpen

-}
udpOpen : Task Error Socket
udpOpen =
    -- Kernel: Create UDP socket
    Elm.Kernel.Socket.udpOpen
        |> Task.map Socket


{-| Bind a UDP socket to an address.

    sock <- Socket.udpOpen
    Socket.udpBind sock (Socket.any 9000)

-}
udpBind : Socket -> Address -> Task Error ()
udpBind (Socket fd) (Address ip port_) =
    -- Kernel: Bind UDP socket
    Elm.Kernel.Socket.udpBind fd ip port_



-- =============================================================================
-- Data Transfer
-- =============================================================================


{-| Send data on a connected socket.

    bytesSent <- Socket.send sock "Hello, server!"

-}
send : Socket -> String -> Task Error Int
send (Socket fd) data =
    -- Kernel: Send data
    Elm.Kernel.Socket.send fd data


{-| Send data to a specific address (UDP).

    Socket.sendTo sock (Socket.address "192.168.1.255" 9000) "Broadcast!"

-}
sendTo : Socket -> Address -> String -> Task Error Int
sendTo (Socket fd) (Address ip port_) data =
    -- Kernel: Sendto
    Elm.Kernel.Socket.sendTo fd ip port_ data


{-| Receive data from a socket.

    data <- Socket.receive sock 1024

-}
receive : Socket -> Int -> Task Error String
receive (Socket fd) maxBytes =
    -- Kernel: Recv
    Elm.Kernel.Socket.receive fd maxBytes


{-| Receive data with sender address (UDP).

    (data, sender) <- Socket.receiveFrom sock 1024

-}
receiveFrom : Socket -> Int -> Task Error ( String, Address )
receiveFrom (Socket fd) maxBytes =
    -- Kernel: Recvfrom
    Elm.Kernel.Socket.receiveFrom fd maxBytes
        |> Task.map (\( data, ip, port_ ) -> ( data, Address ip port_ ))



-- =============================================================================
-- Socket Control
-- =============================================================================


{-| Close a socket.
-}
close : Socket -> Task Error ()
close (Socket fd) =
    -- Kernel: Close socket
    Elm.Kernel.Socket.close fd


{-| Shutdown mode for half-close.
-}
type ShutdownMode
    = Read
    | Write
    | Both


{-| Shutdown part of a socket connection.

    Socket.shutdown sock Write  -- No more sending

-}
shutdown : Socket -> ShutdownMode -> Task Error ()
shutdown (Socket fd) mode =
    let
        modeInt =
            case mode of
                Read ->
                    0

                Write ->
                    1

                Both ->
                    2
    in
    -- Kernel: Shutdown
    Elm.Kernel.Socket.shutdown fd modeInt


{-| Socket options.
-}
type Option
    = ReuseAddr Bool
    | KeepAlive Bool
    | NoDelay Bool -- TCP_NODELAY
    | Broadcast Bool -- For UDP
    | RecvTimeout Int -- Milliseconds
    | SendTimeout Int -- Milliseconds
    | RecvBuffer Int -- Buffer size
    | SendBuffer Int -- Buffer size


{-| Set a socket option.

    Socket.setOption sock (ReuseAddr True)
    Socket.setOption sock (NoDelay True)

-}
setOption : Socket -> Option -> Task Error ()
setOption (Socket fd) option =
    let
        ( optType, optValue ) =
            case option of
                ReuseAddr v ->
                    ( 0, boolToInt v )

                KeepAlive v ->
                    ( 1, boolToInt v )

                NoDelay v ->
                    ( 2, boolToInt v )

                Broadcast v ->
                    ( 3, boolToInt v )

                RecvTimeout ms ->
                    ( 4, ms )

                SendTimeout ms ->
                    ( 5, ms )

                RecvBuffer size ->
                    ( 6, size )

                SendBuffer size ->
                    ( 7, size )
    in
    -- Kernel: setsockopt
    Elm.Kernel.Socket.setOption fd optType optValue


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0



-- =============================================================================
-- Subscriptions
-- =============================================================================


{-| Subscribe to new connections on a listening socket.

    subscriptions model =
        Socket.onConnection model.serverSocket ClientConnected

-}
onConnection : Socket -> (Socket -> msg) -> Sub msg
onConnection (Socket fd) toMsg =
    -- Kernel: Accept subscription
    Elm.Kernel.Socket.onConnection fd (\clientFd -> toMsg (Socket clientFd))


{-| Subscribe to incoming data on a socket.

    subscriptions model =
        Socket.onData model.clientSocket DataReceived

-}
onData : Socket -> (String -> msg) -> Sub msg
onData (Socket fd) toMsg =
    -- Kernel: Recv subscription
    Elm.Kernel.Socket.onData fd toMsg
