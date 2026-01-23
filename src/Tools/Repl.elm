module Tools.Repl exposing
    ( Model, Msg, init, update, view
    , ReplState, Command(..), parseCommand
    , evaluate, formatResult
    )

{-| REPL - Interactive Elm evaluation.

The REPL (Read-Eval-Print-Loop) allows interactive evaluation of Elm
expressions and definitions.

@docs Model, Msg, init, update, view
@docs ReplState, Command, parseCommand
@docs evaluate, formatResult

-}

import AST.Source as Src exposing (Located(..))
import Dict exposing (Dict)
import Parse.Module as Module
import Tools.Highlight as Highlight


{-| REPL state.
-}
type alias ReplState =
    { definitions : Dict String Definition
    , imports : List String
    , history : List HistoryEntry
    , counter : Int
    }


{-| A definition in the REPL environment.
-}
type alias Definition =
    { name : String
    , value : String
    , type_ : Maybe String
    }


{-| A history entry.
-}
type alias HistoryEntry =
    { input : String
    , output : String
    , isError : Bool
    }


{-| REPL model for Elm Architecture.
-}
type alias Model =
    { state : ReplState
    , currentInput : String
    , multilineMode : Bool
    , multilineBuffer : String
    }


{-| REPL messages.
-}
type Msg
    = InputChanged String
    | Submit
    | Clear
    | ToggleMultiline


{-| Initialize the REPL.
-}
init : Model
init =
    { state = initState
    , currentInput = ""
    , multilineMode = False
    , multilineBuffer = ""
    }


initState : ReplState
initState =
    { definitions = Dict.empty
    , imports = [ "Basics", "List", "Maybe", "Result", "String", "Tuple" ]
    , history = []
    , counter = 0
    }


{-| Update the REPL.
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged input ->
            { model | currentInput = input }

        Submit ->
            let
                input =
                    if model.multilineMode then
                        model.multilineBuffer ++ "\n" ++ model.currentInput

                    else
                        model.currentInput

                ( newState, output ) =
                    evaluate model.state input
            in
            { model
                | state = newState
                , currentInput = ""
                , multilineMode = False
                , multilineBuffer = ""
            }

        Clear ->
            { model | state = initState }

        ToggleMultiline ->
            if model.multilineMode then
                { model
                    | multilineMode = False
                    , multilineBuffer = ""
                }

            else
                { model
                    | multilineMode = True
                    , multilineBuffer = model.currentInput
                    , currentInput = ""
                }


{-| View the REPL (as text output).
-}
view : Model -> String
view model =
    let
        historyStr =
            model.state.history
                |> List.reverse
                |> List.map viewHistoryEntry
                |> String.join "\n"

        prompt =
            if model.multilineMode then
                "... "

            else
                "> "

        definitions =
            if Dict.isEmpty model.state.definitions then
                ""

            else
                "-- Definitions --\n"
                    ++ (Dict.values model.state.definitions
                            |> List.map (\d -> d.name ++ " = " ++ d.value)
                            |> String.join "\n"
                       )
                    ++ "\n\n"
    in
    definitions ++ historyStr ++ "\n" ++ prompt ++ model.currentInput


viewHistoryEntry : HistoryEntry -> String
viewHistoryEntry entry =
    let
        prefix =
            if entry.isError then
                "-- ERROR --\n"

            else
                ""
    in
    "> " ++ entry.input ++ "\n" ++ prefix ++ entry.output



-- COMMANDS


{-| REPL commands.
-}
type Command
    = Eval String
    | Define String String
    | Import String
    | TypeOf String
    | Help
    | Reset
    | Quit


{-| Parse a REPL command.
-}
parseCommand : String -> Command
parseCommand input =
    let
        trimmed =
            String.trim input
    in
    if String.startsWith ":help" trimmed then
        Help

    else if String.startsWith ":reset" trimmed then
        Reset

    else if String.startsWith ":quit" trimmed || String.startsWith ":q" trimmed then
        Quit

    else if String.startsWith ":t " trimmed then
        TypeOf (String.dropLeft 3 trimmed)

    else if String.startsWith ":type " trimmed then
        TypeOf (String.dropLeft 6 trimmed)

    else if String.startsWith "import " trimmed then
        Import (String.dropLeft 7 trimmed)

    else if String.contains "=" trimmed && not (String.startsWith "=" trimmed) then
        let
            parts =
                String.split "=" trimmed
        in
        case parts of
            name :: rest ->
                Define (String.trim name) (String.trim (String.join "=" rest))

            _ ->
                Eval trimmed

    else
        Eval trimmed



-- EVALUATION


{-| Evaluate input in the REPL.
-}
evaluate : ReplState -> String -> ( ReplState, String )
evaluate state input =
    let
        command =
            parseCommand input
    in
    case command of
        Help ->
            ( addHistory state input helpText False
            , helpText
            )

        Reset ->
            ( initState
            , "-- REPL reset --"
            )

        Quit ->
            ( state, "-- Goodbye! --" )

        Import moduleName ->
            let
                newState =
                    { state | imports = moduleName :: state.imports }
            in
            ( addHistory newState input ("Imported " ++ moduleName) False
            , "Imported " ++ moduleName
            )

        TypeOf expr ->
            -- Would need type inference to implement properly
            ( addHistory state input "(type inference not yet implemented)" False
            , "(type inference not yet implemented)"
            )

        Define name value ->
            let
                def =
                    { name = name
                    , value = value
                    , type_ = Nothing
                    }

                newState =
                    { state | definitions = Dict.insert name def state.definitions }

                output =
                    name ++ " = " ++ value
            in
            ( addHistory newState input output False
            , output
            )

        Eval expr ->
            evaluateExpression state expr


evaluateExpression : ReplState -> String -> ( ReplState, String )
evaluateExpression state expr =
    -- Wrap the expression in a module to parse it
    let
        moduleCode =
            generateModuleCode state expr

        parseResult =
            Module.parse moduleCode
    in
    case parseResult of
        Ok mod ->
            -- Extract the result
            let
                output =
                    "-- Parsed successfully --\n" ++ expr
            in
            ( addHistory state expr output False
            , output
            )

        Err errors ->
            let
                errorMsg =
                    "Parse error: " ++ Debug.toString errors
            in
            ( addHistory state expr errorMsg True
            , errorMsg
            )


generateModuleCode : ReplState -> String -> String
generateModuleCode state expr =
    let
        imports =
            List.map (\m -> "import " ++ m) state.imports
                |> String.join "\n"

        definitions =
            Dict.values state.definitions
                |> List.map (\d -> d.name ++ " = " ++ d.value)
                |> String.join "\n\n"

        resultName =
            "_result_" ++ String.fromInt state.counter
    in
    """module REPL exposing (..)

""" ++ imports ++ """

""" ++ definitions ++ """

""" ++ resultName ++ """ = """ ++ expr


addHistory : ReplState -> String -> String -> Bool -> ReplState
addHistory state input output isError =
    { state
        | history = { input = input, output = output, isError = isError } :: state.history
        , counter = state.counter + 1
    }


{-| Format a result value for display.
-}
formatResult : String -> String -> String
formatResult typeName value =
    value ++ " : " ++ typeName


helpText : String
helpText =
    """REPL Commands:
  :help          Show this help message
  :reset         Reset the REPL state
  :quit, :q      Exit the REPL
  :type <expr>   Show the type of an expression
  :t <expr>      Short form of :type

Usage:
  - Enter expressions to evaluate them
  - Define values with: name = expression
  - Import modules with: import ModuleName

Examples:
  > 1 + 2
  3 : Int

  > greet name = "Hello, " ++ name
  greet = <function>

  > greet "World"
  "Hello, World" : String
"""
