module Tools.Init exposing
    ( ProjectConfig, Template(..)
    , defaultConfig, applicationConfig, packageConfig
    , generateElmJson, generateMainElm, generateGitignore, generateReadme
    , allFiles
    )

{-| Project Scaffolding - Initialize new tcelm projects.

@docs ProjectConfig, Template
@docs defaultConfig, applicationConfig, packageConfig
@docs generateElmJson, generateMainElm, generateGitignore, generateReadme
@docs allFiles

-}


{-| Project configuration.
-}
type alias ProjectConfig =
    { name : String
    , version : String
    , summary : String
    , license : String
    , author : String
    , template : Template
    , elmVersion : String
    , target : Target
    }


{-| Project template type.
-}
type Template
    = Application
    | Package
    | RtemsApplication
    | NativeApplication


{-| Compilation target.
-}
type Target
    = TargetRtems
    | TargetNative
    | TargetTcc


{-| Default configuration for a new project.
-}
defaultConfig : ProjectConfig
defaultConfig =
    { name = "my-project"
    , version = "1.0.0"
    , summary = "A tcelm project"
    , license = "MIT"
    , author = ""
    , template = Application
    , elmVersion = "0.19.1"
    , target = TargetNative
    }


{-| Application configuration.
-}
applicationConfig : String -> ProjectConfig
applicationConfig name =
    { defaultConfig | name = name, template = Application }


{-| Package configuration.
-}
packageConfig : String -> ProjectConfig
packageConfig name =
    { defaultConfig | name = name, template = Package }



-- FILE GENERATION


{-| Generate all project files.
-}
allFiles : ProjectConfig -> List ( String, String )
allFiles config =
    [ ( "elm.json", generateElmJson config )
    , ( "src/Main.elm", generateMainElm config )
    , ( ".gitignore", generateGitignore config )
    , ( "README.md", generateReadme config )
    ]
        ++ templateSpecificFiles config


templateSpecificFiles : ProjectConfig -> List ( String, String )
templateSpecificFiles config =
    case config.template of
        RtemsApplication ->
            [ ( "Makefile", generateRtemsMakefile config )
            , ( "src/Ports.elm", generatePortsModule config )
            ]

        NativeApplication ->
            [ ( "Makefile", generateNativeMakefile config )
            ]

        _ ->
            []


{-| Generate elm.json content.
-}
generateElmJson : ProjectConfig -> String
generateElmJson config =
    case config.template of
        Package ->
            generatePackageElmJson config

        _ ->
            generateApplicationElmJson config


generateApplicationElmJson : ProjectConfig -> String
generateApplicationElmJson config =
    """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": \"""" ++ config.elmVersion ++ """",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            "elm/json": "1.1.4"
        },
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


generatePackageElmJson : ProjectConfig -> String
generatePackageElmJson config =
    """{
    "type": "package",
    "name": \"""" ++ config.author ++ "/" ++ config.name ++ """",
    "summary": \"""" ++ config.summary ++ """",
    "license": \"""" ++ config.license ++ """",
    "version": \"""" ++ config.version ++ """",
    "exposed-modules": [
        "Main"
    ],
    "elm-version": "0.19.0 <= v < 0.20.0",
    "dependencies": {
        "elm/core": "1.0.0 <= v < 2.0.0"
    },
    "test-dependencies": {}
}
"""


{-| Generate Main.elm content.
-}
generateMainElm : ProjectConfig -> String
generateMainElm config =
    case config.template of
        Application ->
            generateApplicationMain config

        Package ->
            generatePackageMain config

        RtemsApplication ->
            generateRtemsMain config

        NativeApplication ->
            generateNativeMain config


generateApplicationMain : ProjectConfig -> String
generateApplicationMain _ =
    """module Main exposing (main)

{-| Main entry point for the application.
-}


main : Program () () ()
main =
    Platform.worker
        { init = \\_ -> ( (), Cmd.none )
        , update = \\_ _ -> ( (), Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }
"""


generatePackageMain : ProjectConfig -> String
generatePackageMain config =
    """module Main exposing (..)

{-| """ ++ config.summary ++ """

# Main API

@docs example

-}


{-| An example function.
-}
example : Int -> Int
example n =
    n + 1
"""


generateRtemsMain : ProjectConfig -> String
generateRtemsMain _ =
    """module Main exposing (main)

{-| Main entry point for RTEMS application.
-}

import Ports


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { counter : Int
    }


type Msg
    = Tick
    | Reset


init : () -> ( Model, Cmd Msg )
init _ =
    ( { counter = 0 }
    , Ports.log "Application started"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | counter = model.counter + 1 }
            , Ports.log ("Counter: " ++ String.fromInt model.counter)
            )

        Reset ->
            ( { model | counter = 0 }
            , Ports.log "Counter reset"
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.onTick (\\_ -> Tick)
"""


generateNativeMain : ProjectConfig -> String
generateNativeMain _ =
    """module Main exposing (main)

{-| Main entry point for native application.
-}


main : Program () () ()
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( (), Cmd () )
init _ =
    ( (), Cmd.none )


update : () -> () -> ( (), Cmd () )
update _ _ =
    ( (), Cmd.none )


subscriptions : () -> Sub ()
subscriptions _ =
    Sub.none
"""


generatePortsModule : ProjectConfig -> String
generatePortsModule _ =
    """port module Ports exposing (log, onTick)

{-| Ports for RTEMS communication.
-}


port log : String -> Cmd msg


port onTick : (() -> msg) -> Sub msg
"""


{-| Generate .gitignore content.
-}
generateGitignore : ProjectConfig -> String
generateGitignore _ =
    """# Elm
elm-stuff/
*.js

# Build outputs
build/
output/
*.o
*.elf

# IDE
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db
"""


{-| Generate README.md content.
-}
generateReadme : ProjectConfig -> String
generateReadme config =
    """# """ ++ config.name ++ """

""" ++ config.summary ++ """

## Getting Started

### Prerequisites

- tcelm compiler
- """ ++ targetPrereqs config.target ++ """

### Building

```bash
tcelm make src/Main.elm
```

### Running

```bash
""" ++ targetRunCommand config.target ++ """
```

## License

""" ++ config.license ++ """
"""


targetPrereqs : Target -> String
targetPrereqs target =
    case target of
        TargetRtems ->
            "RTEMS toolchain"

        TargetNative ->
            "GCC or Clang"

        TargetTcc ->
            "TCC (Tiny C Compiler)"


targetRunCommand : Target -> String
targetRunCommand target =
    case target of
        TargetRtems ->
            "# Deploy to RTEMS target"

        TargetNative ->
            "./build/main"

        TargetTcc ->
            "./build/main"


generateRtemsMakefile : ProjectConfig -> String
generateRtemsMakefile _ =
    """# RTEMS Makefile for tcelm project

TCELM = tcelm
TCC = tcc
TARGET = i386-rtems-nuc

SRC = src/Main.elm
OUTPUT = build/main.c
BINARY = build/main.elf

.PHONY: all clean

all: $(BINARY)

$(OUTPUT): $(SRC)
\t@mkdir -p build
\t$(TCELM) make --target=$(TARGET) $(SRC) --output=$(OUTPUT)

$(BINARY): $(OUTPUT)
\t$(TCC) -o $(BINARY) $(OUTPUT)

clean:
\trm -rf build/
"""


generateNativeMakefile : ProjectConfig -> String
generateNativeMakefile _ =
    """# Native Makefile for tcelm project

TCELM = tcelm
CC = gcc

SRC = src/Main.elm
OUTPUT = build/main.c
BINARY = build/main

.PHONY: all clean run

all: $(BINARY)

$(OUTPUT): $(SRC)
\t@mkdir -p build
\t$(TCELM) make --target=native $(SRC) --output=$(OUTPUT)

$(BINARY): $(OUTPUT)
\t$(CC) -o $(BINARY) $(OUTPUT)

run: $(BINARY)
\t./$(BINARY)

clean:
\trm -rf build/
"""
