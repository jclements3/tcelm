-- expect: 3
module Test exposing (main)

type alias State s a = s -> ( a, s )

pure : a -> State s a
pure x = \s -> ( x, s )

andThen : (a -> State s b) -> State s a -> State s b
andThen f computation =
    \s ->
        let
            ( a, s1 ) = computation s
            ( b, s2 ) = f a s1
        in
        ( b, s2 )

modify : (s -> s) -> State s ()
modify f = \s -> ( (), f s )

get : State s s
get = \s -> ( s, s )

type alias Stack = List Int

push : Int -> State Stack ()
push x = modify (\stack -> x :: stack)

pop : State Stack (Maybe Int)
pop =
    \stack ->
        case stack of
            [] -> ( Nothing, [] )
            x :: xs -> ( Just x, xs )

stackOps : State Stack (Maybe Int)
stackOps =
    pure ()
        |> andThen (\_ -> push 1)
        |> andThen (\_ -> push 2)
        |> andThen (\_ -> push 3)
        |> andThen (\_ -> pop)

result : ( Maybe Int, Stack )
result = stackOps []

main =
    case Tuple.first result of
        Just n -> n
        Nothing -> -1
