module ShellScript exposing (main)

{-| Turtle/Shelly-style shell scripting example.

This demonstrates type-safe shell scripting in Elm for RTEMS,
inspired by Haskell's Turtle and Shelly libraries.

-}

import Rtems
import Rtems.Shell exposing (..)


-- ============================================================================
-- EXAMPLE 1: Basic file operations
-- ============================================================================


{-| Create a directory structure and some files.
-}
setupProject : Cmd msg
setupProject =
    let
        projectDir =
            fromString "/tmp/myproject"
    in
    Cmd.batch
        [ mkdirTree (projectDir </> fromString "src")
        , mkdirTree (projectDir </> fromString "tests")
        , mkdirTree (projectDir </> fromString "docs")
        , writeFile (projectDir </> fromString "README.md")
            "# My Project\n\nA tcelm project.\n"
        , writeFile (projectDir </> fromString "src/Main.elm")
            "module Main exposing (main)\n\nmain = ()\n"
        ]



-- ============================================================================
-- EXAMPLE 2: Finding and processing files
-- ============================================================================


{-| Find all Elm files and count their lines.
-}
countElmLines : FilePath -> Int
countElmLines dir =
    find (glob "*.elm") dir
        |> select cat
        |> fold countLines


{-| Find all TODO comments in source files.
-}
findTodos : FilePath -> Shell Line
findTodos dir =
    find (glob "*.elm") dir
        |> select cat
        |> grep "TODO"


{-| List all Elm files larger than 1000 bytes.
-}
largeElmFiles : FilePath -> Shell FilePath
largeElmFiles dir =
    find (glob "*.elm") dir
        |> select
            (\path ->
                if fileSize path > 1000 then
                    single path

                else
                    empty
            )



-- ============================================================================
-- EXAMPLE 3: Text processing pipelines
-- ============================================================================


{-| Process a log file: find errors, extract timestamps, sort uniquely.
-}
processLogFile : FilePath -> Shell Line
processLogFile logFile =
    cat logFile
        |> grep "ERROR"
        |> cut " " 0  -- Extract first field (timestamp)
        |> sort
        |> uniq


{-| Transform a CSV file: extract column, uppercase it.
-}
transformCsv : FilePath -> Shell Line
transformCsv csvFile =
    cat csvFile
        |> cut "," 1  -- Extract second column
        |> sed "a" "A"
        |> sed "e" "E"
        |> sed "i" "I"
        |> sed "o" "O"
        |> sed "u" "U"



-- ============================================================================
-- EXAMPLE 4: Process execution
-- ============================================================================


{-| Run a build command and capture output.
-}
runBuild : String -> { stdout : String, stderr : String, exit : ExitCode }
runBuild target =
    procStrictWithErr "make" [ target ]


{-| Check if a command is available.
-}
commandAvailable : String -> Bool
commandAvailable cmd =
    case which cmd of
        Just _ ->
            True

        Nothing ->
            False


{-| Run a series of commands, stopping on first failure.
-}
runPipeline : List String -> ExitCode
runPipeline commands =
    case commands of
        [] ->
            ExitSuccess

        cmd :: rest ->
            case procExit "sh" [ "-c", cmd ] of
                ExitSuccess ->
                    runPipeline rest

                failure ->
                    failure



-- ============================================================================
-- EXAMPLE 5: Environment and system info
-- ============================================================================


{-| Get system information.
-}
systemInfo : String
systemInfo =
    let
        host =
            hostname ()

        dateStr =
            date ()

        user =
            env "USER" |> Maybe.withDefault "unknown"

        homeDir =
            env "HOME" |> Maybe.withDefault "/"
    in
    "Host: "
        ++ host
        ++ "\nDate: "
        ++ dateStr
        ++ "\nUser: "
        ++ user
        ++ "\nHome: "
        ++ homeDir



-- ============================================================================
-- EXAMPLE 6: Backup script
-- ============================================================================


{-| Backup all .elm files to a backup directory.
-}
backupElmFiles : FilePath -> FilePath -> Cmd msg
backupElmFiles srcDir backupDir =
    let
        timestamp =
            date ()
                |> String.replace " " "_"
                |> String.replace ":" "-"

        backupPath =
            backupDir </> fromString ("backup_" ++ timestamp)
    in
    Cmd.batch
        [ mkdirTree backupPath
        , find (glob "*.elm") srcDir
            |> select
                (\src ->
                    let
                        dst =
                            backupPath </> filename src
                    in
                    single (cp src dst)
                )
            |> run
        ]



-- ============================================================================
-- EXAMPLE 7: Using folds for aggregation
-- ============================================================================


{-| Count files by extension in a directory.
-}
countByExtension : FilePath -> List ( String, Int )
countByExtension dir =
    lsTree dir
        |> runFold
            (Fold
                (\acc path ->
                    case extension path of
                        Just ext ->
                            updateCount ext acc

                        Nothing ->
                            acc
                )
                []
                identity
            )


updateCount : String -> List ( String, Int ) -> List ( String, Int )
updateCount ext counts =
    case counts of
        [] ->
            [ ( ext, 1 ) ]

        ( e, n ) :: rest ->
            if e == ext then
                ( e, n + 1 ) :: rest

            else
                ( e, n ) :: updateCount ext rest



-- ============================================================================
-- MAIN
-- ============================================================================


type alias Model =
    { initialized : Bool }


type Msg
    = Init
    | Done


init : Model
init =
    { initialized = False }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            ( { model | initialized = True }
            , Cmd.batch
                [ echo "=== Shell Script Demo ==="
                , echo ""
                , echo "System Info:"
                , echo systemInfo
                , echo ""
                , echo "Setting up project..."
                , setupProject
                , echo "Project created at /tmp/myproject"
                , echo ""
                , echo "Done!"
                ]
            )

        Done ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.initialized then
        Rtems.delay 100 Init

    else
        Sub.none


main : Program () Model Msg
main =
    Rtems.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
