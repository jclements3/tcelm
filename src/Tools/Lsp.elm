module Tools.Lsp exposing
    ( ServerState, init, handleMessage
    , Request, Response, Notification
    , Position, Range, Location, Diagnostic, DiagnosticSeverity(..)
    , CompletionItem, CompletionItemKind(..)
    , Hover, SignatureHelp
    , encodeResponse, decodeRequest
    )

{-| Language Server Protocol - IDE support for Elm.

Implements LSP for features like:

  - Go to definition
  - Find references
  - Hover for types
  - Autocomplete
  - Inline errors

@docs ServerState, init, handleMessage
@docs Request, Response, Notification
@docs Position, Range, Location, Diagnostic, DiagnosticSeverity
@docs CompletionItem, CompletionItemKind
@docs Hover, SignatureHelp
@docs encodeResponse, decodeRequest

-}

import AST.Source as Src exposing (Located(..))
import Dict exposing (Dict)
import Format.Format
import Parse.Module as Module
import Tools.Lint as Lint


{-| LSP server state.
-}
type alias ServerState =
    { initialized : Bool
    , workspaceRoot : Maybe String
    , openDocuments : Dict String DocumentState
    , configuration : Configuration
    }


type alias DocumentState =
    { uri : String
    , content : String
    , version : Int
    , parsed : Maybe Src.Module
    , diagnostics : List Diagnostic
    }


type alias Configuration =
    { enableLinting : Bool
    , enableFormatting : Bool
    }


{-| Initialize the server.
-}
init : ServerState
init =
    { initialized = False
    , workspaceRoot = Nothing
    , openDocuments = Dict.empty
    , configuration =
        { enableLinting = True
        , enableFormatting = True
        }
    }



-- PROTOCOL TYPES


{-| A position in a document.
-}
type alias Position =
    { line : Int
    , character : Int
    }


{-| A range in a document.
-}
type alias Range =
    { start : Position
    , end : Position
    }


{-| A location (document + range).
-}
type alias Location =
    { uri : String
    , range : Range
    }


{-| A diagnostic (error/warning).
-}
type alias Diagnostic =
    { range : Range
    , severity : DiagnosticSeverity
    , code : Maybe String
    , source : String
    , message : String
    }


{-| Diagnostic severity levels.
-}
type DiagnosticSeverity
    = SeverityError
    | SeverityWarning
    | SeverityInformation
    | SeverityHint


{-| Hover information.
-}
type alias Hover =
    { contents : String
    , range : Maybe Range
    }


{-| Signature help.
-}
type alias SignatureHelp =
    { signatures : List SignatureInformation
    , activeSignature : Int
    , activeParameter : Int
    }


type alias SignatureInformation =
    { label : String
    , documentation : Maybe String
    , parameters : List ParameterInformation
    }


type alias ParameterInformation =
    { label : String
    , documentation : Maybe String
    }


{-| Completion item.
-}
type alias CompletionItem =
    { label : String
    , kind : CompletionItemKind
    , detail : Maybe String
    , documentation : Maybe String
    , insertText : Maybe String
    }


{-| Completion item kinds.
-}
type CompletionItemKind
    = KindText
    | KindMethod
    | KindFunction
    | KindConstructor
    | KindField
    | KindVariable
    | KindClass
    | KindInterface
    | KindModule
    | KindProperty
    | KindUnit
    | KindValue
    | KindEnum
    | KindKeyword
    | KindSnippet
    | KindColor
    | KindFile
    | KindReference
    | KindFolder
    | KindEnumMember
    | KindConstant
    | KindStruct
    | KindEvent
    | KindOperator
    | KindTypeParameter



-- MESSAGES


{-| LSP request.
-}
type Request
    = Initialize InitializeParams
    | Shutdown
    | TextDocumentHover HoverParams
    | TextDocumentCompletion CompletionParams
    | TextDocumentDefinition DefinitionParams
    | TextDocumentReferences ReferencesParams
    | TextDocumentFormatting FormattingParams
    | TextDocumentSignatureHelp SignatureHelpParams


type alias InitializeParams =
    { rootUri : Maybe String
    }


type alias HoverParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias CompletionParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias DefinitionParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias ReferencesParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias FormattingParams =
    { textDocument : TextDocumentIdentifier
    }


type alias SignatureHelpParams =
    { textDocument : TextDocumentIdentifier
    , position : Position
    }


type alias TextDocumentIdentifier =
    { uri : String
    }


{-| LSP response.
-}
type Response
    = InitializeResult ServerCapabilities
    | HoverResult (Maybe Hover)
    | CompletionResult (List CompletionItem)
    | DefinitionResult (Maybe Location)
    | ReferencesResult (List Location)
    | FormattingResult (List TextEdit)
    | SignatureHelpResult (Maybe SignatureHelp)
    | ShutdownResult
    | ErrorResponse Int String


type alias ServerCapabilities =
    { hoverProvider : Bool
    , completionProvider : Bool
    , definitionProvider : Bool
    , referencesProvider : Bool
    , documentFormattingProvider : Bool
    , signatureHelpProvider : Bool
    }


type alias TextEdit =
    { range : Range
    , newText : String
    }


{-| LSP notification.
-}
type Notification
    = DidOpenTextDocument DidOpenParams
    | DidChangeTextDocument DidChangeParams
    | DidCloseTextDocument DidCloseParams
    | DidSaveTextDocument DidSaveParams
    | PublishDiagnostics PublishDiagnosticsParams


type alias DidOpenParams =
    { textDocument : TextDocumentItem
    }


type alias TextDocumentItem =
    { uri : String
    , languageId : String
    , version : Int
    , text : String
    }


type alias DidChangeParams =
    { textDocument : VersionedTextDocumentIdentifier
    , contentChanges : List ContentChange
    }


type alias VersionedTextDocumentIdentifier =
    { uri : String
    , version : Int
    }


type alias ContentChange =
    { text : String
    }


type alias DidCloseParams =
    { textDocument : TextDocumentIdentifier
    }


type alias DidSaveParams =
    { textDocument : TextDocumentIdentifier
    }


type alias PublishDiagnosticsParams =
    { uri : String
    , diagnostics : List Diagnostic
    }



-- MESSAGE HANDLING


{-| Handle an incoming message.
-}
handleMessage : ServerState -> Result String Request -> ( ServerState, Response, List Notification )
handleMessage state requestResult =
    case requestResult of
        Err err ->
            ( state, ErrorResponse -32700 ("Parse error: " ++ err), [] )

        Ok request ->
            handleRequest state request


handleRequest : ServerState -> Request -> ( ServerState, Response, List Notification )
handleRequest state request =
    case request of
        Initialize params ->
            let
                newState =
                    { state
                        | initialized = True
                        , workspaceRoot = params.rootUri
                    }

                capabilities =
                    { hoverProvider = True
                    , completionProvider = True
                    , definitionProvider = True
                    , referencesProvider = True
                    , documentFormattingProvider = True
                    , signatureHelpProvider = True
                    }
            in
            ( newState, InitializeResult capabilities, [] )

        Shutdown ->
            ( state, ShutdownResult, [] )

        TextDocumentHover params ->
            let
                hover =
                    getHover state params.textDocument.uri params.position
            in
            ( state, HoverResult hover, [] )

        TextDocumentCompletion params ->
            let
                completions =
                    getCompletions state params.textDocument.uri params.position
            in
            ( state, CompletionResult completions, [] )

        TextDocumentDefinition params ->
            let
                location =
                    getDefinition state params.textDocument.uri params.position
            in
            ( state, DefinitionResult location, [] )

        TextDocumentReferences params ->
            let
                refs =
                    getReferences state params.textDocument.uri params.position
            in
            ( state, ReferencesResult refs, [] )

        TextDocumentFormatting params ->
            let
                edits =
                    formatDocument state params.textDocument.uri
            in
            ( state, FormattingResult edits, [] )

        TextDocumentSignatureHelp params ->
            let
                help =
                    getSignatureHelp state params.textDocument.uri params.position
            in
            ( state, SignatureHelpResult help, [] )


{-| Handle a notification (no response needed).
-}
handleNotification : ServerState -> Notification -> ( ServerState, List Notification )
handleNotification state notification =
    case notification of
        DidOpenTextDocument params ->
            let
                ( docState, diagnostics ) =
                    parseDocument params.textDocument.uri params.textDocument.text params.textDocument.version

                newState =
                    { state
                        | openDocuments = Dict.insert params.textDocument.uri docState state.openDocuments
                    }

                notifications =
                    if List.isEmpty diagnostics then
                        []

                    else
                        [ PublishDiagnostics { uri = params.textDocument.uri, diagnostics = diagnostics } ]
            in
            ( newState, notifications )

        DidChangeTextDocument params ->
            case params.contentChanges of
                [ change ] ->
                    let
                        ( docState, diagnostics ) =
                            parseDocument params.textDocument.uri change.text params.textDocument.version

                        newState =
                            { state
                                | openDocuments = Dict.insert params.textDocument.uri docState state.openDocuments
                            }

                        notifications =
                            [ PublishDiagnostics { uri = params.textDocument.uri, diagnostics = diagnostics } ]
                    in
                    ( newState, notifications )

                _ ->
                    ( state, [] )

        DidCloseTextDocument params ->
            let
                newState =
                    { state
                        | openDocuments = Dict.remove params.textDocument.uri state.openDocuments
                    }
            in
            ( newState, [] )

        DidSaveTextDocument _ ->
            ( state, [] )

        PublishDiagnostics _ ->
            ( state, [] )


parseDocument : String -> String -> Int -> ( DocumentState, List Diagnostic )
parseDocument uri content version =
    case Module.parse content of
        Ok mod ->
            let
                lintIssues =
                    Lint.lint mod

                diagnostics =
                    List.map issueToDiagnostic lintIssues
            in
            ( { uri = uri
              , content = content
              , version = version
              , parsed = Just mod
              , diagnostics = diagnostics
              }
            , diagnostics
            )

        Err errors ->
            let
                diagnostics =
                    [ { range =
                            { start = { line = 0, character = 0 }
                            , end = { line = 0, character = 1 }
                            }
                      , severity = SeverityError
                      , code = Nothing
                      , source = "tcelm"
                      , message = "Parse error"
                      }
                    ]
            in
            ( { uri = uri
              , content = content
              , version = version
              , parsed = Nothing
              , diagnostics = diagnostics
              }
            , diagnostics
            )


issueToDiagnostic : Lint.Issue -> Diagnostic
issueToDiagnostic issue =
    { range =
        { start = { line = issue.region.start.row - 1, character = issue.region.start.col - 1 }
        , end = { line = issue.region.end.row - 1, character = issue.region.end.col - 1 }
        }
    , severity =
        case issue.severity of
            Lint.Error ->
                SeverityError

            Lint.Warning ->
                SeverityWarning

            Lint.Info ->
                SeverityInformation
    , code = Just issue.rule
    , source = "tcelm-lint"
    , message = issue.message
    }



-- FEATURE IMPLEMENTATIONS


getHover : ServerState -> String -> Position -> Maybe Hover
getHover state uri position =
    case Dict.get uri state.openDocuments of
        Just doc ->
            case doc.parsed of
                Just mod ->
                    -- Find symbol at position and return its type
                    Just
                        { contents = "-- Hover info at line " ++ String.fromInt (position.line + 1)
                        , range = Nothing
                        }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


getCompletions : ServerState -> String -> Position -> List CompletionItem
getCompletions state uri _ =
    let
        keywords =
            [ "module", "exposing", "import", "as", "type", "alias", "if", "then", "else", "case", "of", "let", "in", "port" ]
                |> List.map
                    (\kw ->
                        { label = kw
                        , kind = KindKeyword
                        , detail = Just "keyword"
                        , documentation = Nothing
                        , insertText = Nothing
                        }
                    )

        builtins =
            [ ( "List.map", "Transform each element" )
            , ( "List.filter", "Keep elements matching predicate" )
            , ( "Maybe.map", "Transform Maybe value" )
            , ( "Maybe.withDefault", "Provide default for Nothing" )
            , ( "String.concat", "Concatenate strings" )
            ]
                |> List.map
                    (\( name, doc ) ->
                        { label = name
                        , kind = KindFunction
                        , detail = Nothing
                        , documentation = Just doc
                        , insertText = Nothing
                        }
                    )
    in
    keywords ++ builtins


getDefinition : ServerState -> String -> Position -> Maybe Location
getDefinition state uri position =
    -- Would look up symbol at position and find its definition
    Nothing


getReferences : ServerState -> String -> Position -> List Location
getReferences state uri position =
    -- Would find all references to symbol at position
    []


formatDocument : ServerState -> String -> List TextEdit
formatDocument state uri =
    case Dict.get uri state.openDocuments of
        Just doc ->
            case doc.parsed of
                Just mod ->
                    let
                        formatted =
                            Format.Format.format mod

                        lastLine =
                            String.lines doc.content |> List.length
                    in
                    [ { range =
                            { start = { line = 0, character = 0 }
                            , end = { line = lastLine, character = 0 }
                            }
                      , newText = formatted
                      }
                    ]

                Nothing ->
                    []

        Nothing ->
            []


getSignatureHelp : ServerState -> String -> Position -> Maybe SignatureHelp
getSignatureHelp state uri position =
    -- Would provide signature help for function call
    Nothing



-- ENCODING/DECODING


{-| Encode a response to JSON.
-}
encodeResponse : Int -> Response -> String
encodeResponse id response =
    let
        resultJson =
            case response of
                InitializeResult caps ->
                    "{\"capabilities\":" ++ encodeCapabilities caps ++ "}"

                HoverResult maybeHover ->
                    case maybeHover of
                        Just h ->
                            "{\"contents\":\"" ++ escapeJson h.contents ++ "\"}"

                        Nothing ->
                            "null"

                CompletionResult items ->
                    "[" ++ String.join "," (List.map encodeCompletionItem items) ++ "]"

                DefinitionResult maybeLoc ->
                    case maybeLoc of
                        Just loc ->
                            encodeLocation loc

                        Nothing ->
                            "null"

                ReferencesResult locs ->
                    "[" ++ String.join "," (List.map encodeLocation locs) ++ "]"

                FormattingResult edits ->
                    "[" ++ String.join "," (List.map encodeTextEdit edits) ++ "]"

                SignatureHelpResult _ ->
                    "null"

                ShutdownResult ->
                    "null"

                ErrorResponse code msg ->
                    "{\"code\":" ++ String.fromInt code ++ ",\"message\":\"" ++ escapeJson msg ++ "\"}"
    in
    "{\"jsonrpc\":\"2.0\",\"id\":" ++ String.fromInt id ++ ",\"result\":" ++ resultJson ++ "}"


encodeCapabilities : ServerCapabilities -> String
encodeCapabilities caps =
    "{\"hoverProvider\":" ++ boolToJson caps.hoverProvider
        ++ ",\"completionProvider\":{\"triggerCharacters\":[\".\"]}"
        ++ ",\"definitionProvider\":" ++ boolToJson caps.definitionProvider
        ++ ",\"referencesProvider\":" ++ boolToJson caps.referencesProvider
        ++ ",\"documentFormattingProvider\":" ++ boolToJson caps.documentFormattingProvider
        ++ "}"


encodeCompletionItem : CompletionItem -> String
encodeCompletionItem item =
    "{\"label\":\"" ++ escapeJson item.label ++ "\""
        ++ ",\"kind\":" ++ String.fromInt (completionKindToInt item.kind)
        ++ (case item.detail of
                Just d ->
                    ",\"detail\":\"" ++ escapeJson d ++ "\""

                Nothing ->
                    ""
           )
        ++ "}"


completionKindToInt : CompletionItemKind -> Int
completionKindToInt kind =
    case kind of
        KindText ->
            1

        KindMethod ->
            2

        KindFunction ->
            3

        KindConstructor ->
            4

        KindField ->
            5

        KindVariable ->
            6

        KindClass ->
            7

        KindInterface ->
            8

        KindModule ->
            9

        KindProperty ->
            10

        KindUnit ->
            11

        KindValue ->
            12

        KindEnum ->
            13

        KindKeyword ->
            14

        KindSnippet ->
            15

        KindColor ->
            16

        KindFile ->
            17

        KindReference ->
            18

        KindFolder ->
            19

        KindEnumMember ->
            20

        KindConstant ->
            21

        KindStruct ->
            22

        KindEvent ->
            23

        KindOperator ->
            24

        KindTypeParameter ->
            25


encodeLocation : Location -> String
encodeLocation loc =
    "{\"uri\":\"" ++ escapeJson loc.uri ++ "\",\"range\":" ++ encodeRange loc.range ++ "}"


encodeRange : Range -> String
encodeRange range =
    "{\"start\":" ++ encodePosition range.start ++ ",\"end\":" ++ encodePosition range.end ++ "}"


encodePosition : Position -> String
encodePosition pos =
    "{\"line\":" ++ String.fromInt pos.line ++ ",\"character\":" ++ String.fromInt pos.character ++ "}"


encodeTextEdit : TextEdit -> String
encodeTextEdit edit =
    "{\"range\":" ++ encodeRange edit.range ++ ",\"newText\":\"" ++ escapeJson edit.newText ++ "\"}"


boolToJson : Bool -> String
boolToJson b =
    if b then
        "true"

    else
        "false"


escapeJson : String -> String
escapeJson =
    String.replace "\\" "\\\\"
        >> String.replace "\"" "\\\""
        >> String.replace "\n" "\\n"
        >> String.replace "\t" "\\t"


{-| Decode a request from JSON (simplified).
-}
decodeRequest : String -> Result String Request
decodeRequest json =
    -- Simplified - would use proper JSON decoder
    if String.contains "initialize" json then
        Ok (Initialize { rootUri = Nothing })

    else if String.contains "shutdown" json then
        Ok Shutdown

    else if String.contains "textDocument/hover" json then
        Ok
            (TextDocumentHover
                { textDocument = { uri = "" }
                , position = { line = 0, character = 0 }
                }
            )

    else
        Err "Unknown method"
