module Tools.DevServer exposing
    ( Config, defaultConfig
    , ServerState, init, handleRequest
    , Request, Response, StatusCode(..)
    , FileWatcher, WatchEvent(..)
    , compileOnChange, liveReloadScript
    )

{-| Development Server - Auto-recompile and serve compiled output.

Note: This module provides the logic for a dev server. The actual HTTP
serving and file watching would be handled by a JavaScript/Node.js wrapper.

@docs Config, defaultConfig
@docs ServerState, init, handleRequest
@docs Request, Response, StatusCode
@docs FileWatcher, WatchEvent
@docs compileOnChange, liveReloadScript

-}

import Dict exposing (Dict)


{-| Server configuration.
-}
type alias Config =
    { port_ : Int
    , host : String
    , sourceDir : String
    , outputDir : String
    , liveReload : Bool
    , openBrowser : Bool
    }


{-| Default server configuration.
-}
defaultConfig : Config
defaultConfig =
    { port_ = 8000
    , host = "localhost"
    , sourceDir = "src"
    , outputDir = "build"
    , liveReload = True
    , openBrowser = True
    }


{-| Server state.
-}
type alias ServerState =
    { config : Config
    , compiledModules : Dict String CompiledModule
    , lastCompileError : Maybe String
    , clients : List ClientId
    , buildNumber : Int
    }


type alias CompiledModule =
    { source : String
    , output : String
    , lastModified : Int
    }


type alias ClientId =
    Int


{-| Initialize the server.
-}
init : Config -> ServerState
init config =
    { config = config
    , compiledModules = Dict.empty
    , lastCompileError = Nothing
    , clients = []
    , buildNumber = 0
    }



-- HTTP HANDLING


{-| HTTP request.
-}
type alias Request =
    { method : String
    , path : String
    , headers : Dict String String
    , body : String
    }


{-| HTTP response.
-}
type alias Response =
    { statusCode : StatusCode
    , headers : Dict String String
    , body : String
    }


{-| HTTP status codes.
-}
type StatusCode
    = Ok200
    | NotFound404
    | InternalError500
    | NotModified304


{-| Handle an HTTP request.
-}
handleRequest : ServerState -> Request -> ( ServerState, Response )
handleRequest state request =
    case request.path of
        "/" ->
            ( state, serveIndex state )

        "/_compile" ->
            handleCompileRequest state request

        "/_live-reload" ->
            handleLiveReloadRequest state request

        path ->
            if String.endsWith ".elm" path then
                serveElmFile state path

            else if String.endsWith ".js" path then
                serveJsFile state path

            else if String.endsWith ".css" path then
                serveCssFile state path

            else
                ( state, notFound path )


serveIndex : ServerState -> Response
serveIndex state =
    let
        errorOverlay =
            case state.lastCompileError of
                Just err ->
                    "<div id=\"error-overlay\" style=\"" ++ errorOverlayStyle ++ "\">"
                        ++ "<pre>" ++ escapeHtml err ++ "</pre>"
                        ++ "</div>"

                Nothing ->
                    ""

        liveReloadTag =
            if state.config.liveReload then
                "<script>" ++ liveReloadScript ++ "</script>"

            else
                ""
    in
    { statusCode = Ok200
    , headers = Dict.singleton "Content-Type" "text/html"
    , body =
        """<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>tcelm Dev Server</title>
    <style>
        body { font-family: sans-serif; padding: 20px; }
        h1 { color: #1293d8; }
        .module { margin: 10px 0; padding: 10px; background: #f5f5f5; border-radius: 5px; }
        .module a { color: #1293d8; text-decoration: none; }
        .module a:hover { text-decoration: underline; }
    </style>
</head>
<body>
    <h1>tcelm Dev Server</h1>
    <p>Build #""" ++ String.fromInt state.buildNumber ++ """</p>
    """ ++ errorOverlay ++ """
    <h2>Modules</h2>
    """ ++ moduleList state ++ """
    """ ++ liveReloadTag ++ """
</body>
</html>
"""
    }


moduleList : ServerState -> String
moduleList state =
    Dict.keys state.compiledModules
        |> List.map (\m -> "<div class=\"module\"><a href=\"/" ++ m ++ ".js\">" ++ m ++ "</a></div>")
        |> String.join "\n"


handleCompileRequest : ServerState -> Request -> ( ServerState, Response )
handleCompileRequest state request =
    -- Would trigger recompilation
    let
        newState =
            { state | buildNumber = state.buildNumber + 1 }
    in
    ( newState
    , { statusCode = Ok200
      , headers = Dict.singleton "Content-Type" "application/json"
      , body = "{\"success\": true, \"build\": " ++ String.fromInt newState.buildNumber ++ "}"
      }
    )


handleLiveReloadRequest : ServerState -> Request -> ( ServerState, Response )
handleLiveReloadRequest state _ =
    -- Server-Sent Events for live reload
    ( state
    , { statusCode = Ok200
      , headers =
            Dict.fromList
                [ ( "Content-Type", "text/event-stream" )
                , ( "Cache-Control", "no-cache" )
                , ( "Connection", "keep-alive" )
                ]
      , body = "data: {\"build\": " ++ String.fromInt state.buildNumber ++ "}\n\n"
      }
    )


serveElmFile : ServerState -> String -> ( ServerState, Response )
serveElmFile state path =
    let
        moduleName =
            String.dropLeft 1 path |> String.replace ".elm" ""
    in
    case Dict.get moduleName state.compiledModules of
        Just mod ->
            ( state
            , { statusCode = Ok200
              , headers = Dict.singleton "Content-Type" "text/plain"
              , body = mod.source
              }
            )

        Nothing ->
            ( state, notFound path )


serveJsFile : ServerState -> String -> ( ServerState, Response )
serveJsFile state path =
    let
        moduleName =
            String.dropLeft 1 path |> String.replace ".js" ""
    in
    case Dict.get moduleName state.compiledModules of
        Just mod ->
            ( state
            , { statusCode = Ok200
              , headers = Dict.singleton "Content-Type" "application/javascript"
              , body = mod.output
              }
            )

        Nothing ->
            ( state, notFound path )


serveCssFile : ServerState -> String -> ( ServerState, Response )
serveCssFile state path =
    ( state, notFound path )


notFound : String -> Response
notFound path =
    { statusCode = NotFound404
    , headers = Dict.singleton "Content-Type" "text/html"
    , body = "<h1>404 Not Found</h1><p>" ++ escapeHtml path ++ "</p>"
    }


errorOverlayStyle : String
errorOverlayStyle =
    "position:fixed;top:0;left:0;right:0;bottom:0;background:rgba(0,0,0,0.9);color:#ff5555;padding:40px;overflow:auto;font-family:monospace;white-space:pre-wrap;"



-- FILE WATCHING


{-| File watcher state.
-}
type alias FileWatcher =
    { watchedPaths : List String
    , debounceMs : Int
    }


{-| File watch events.
-}
type WatchEvent
    = FileChanged String
    | FileCreated String
    | FileDeleted String


{-| Compile on file change.
-}
compileOnChange : WatchEvent -> ServerState -> ( ServerState, List String )
compileOnChange event state =
    case event of
        FileChanged path ->
            if String.endsWith ".elm" path then
                ( { state | buildNumber = state.buildNumber + 1 }
                , [ "Compiling " ++ path ]
                )

            else
                ( state, [] )

        FileCreated path ->
            if String.endsWith ".elm" path then
                ( { state | buildNumber = state.buildNumber + 1 }
                , [ "New file: " ++ path ]
                )

            else
                ( state, [] )

        FileDeleted path ->
            ( state, [ "Deleted: " ++ path ] )


{-| JavaScript for live reload client.
-}
liveReloadScript : String
liveReloadScript =
    """
(function() {
    var currentBuild = 0;
    var retryCount = 0;
    var maxRetries = 10;

    function connect() {
        var evtSource = new EventSource('/_live-reload');

        evtSource.onmessage = function(event) {
            var data = JSON.parse(event.data);
            if (currentBuild === 0) {
                currentBuild = data.build;
            } else if (data.build > currentBuild) {
                console.log('[tcelm] Reloading...');
                window.location.reload();
            }
            retryCount = 0;
        };

        evtSource.onerror = function() {
            evtSource.close();
            if (retryCount < maxRetries) {
                retryCount++;
                console.log('[tcelm] Connection lost, retrying in 1s...');
                setTimeout(connect, 1000);
            }
        };
    }

    connect();
    console.log('[tcelm] Live reload enabled');
})();
"""


escapeHtml : String -> String
escapeHtml =
    String.replace "&" "&amp;"
        >> String.replace "<" "&lt;"
        >> String.replace ">" "&gt;"
