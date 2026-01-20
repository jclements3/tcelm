module Rtems.Shell exposing
    ( -- Paths
      FilePath
    , fromString
    , toString
    , (</>)
    , parent
    , filename
    , extension
    , hasExtension
    , dropExtension
    , addExtension
    , splitDirectories
    , joinPath

    -- Shell Monad
    , Shell
    , run
    , runFold
    , single
    , select
    , empty
    , (<|>)

    -- Directory Operations
    , pwd
    , cd
    , home
    , ls
    , lsTree
    , mkdir
    , mkdirTree
    , rmdir
    , rmtree

    -- File Operations
    , touch
    , cp
    , mv
    , rm
    , cat
    , readFile
    , writeFile
    , appendFile
    , fileExists
    , dirExists
    , isFile
    , isDir
    , fileSize

    -- Text Processing (Line-based)
    , Line
    , lineToText
    , textToLine
    , input
    , stdin
    , output
    , stdout
    , stderr
    , grep
    , sed
    , cut
    , sort
    , uniq
    , head
    , tail
    , wc

    -- Process Execution
    , proc
    , shell
    , inproc
    , inshell
    , ExitCode(..)
    , procExit
    , procStrict
    , procStrictWithErr

    -- Patterns (Glob)
    , Pattern
    , match
    , glob
    , find

    -- Utilities
    , echo
    , printf
    , date
    , sleep
    , env
    , export
    , which
    , hostname

    -- Folding
    , Fold
    , fold
    , foldIO
    , list
    , countLines
    , head_
    , last_
    )

{-| Type-safe shell scripting for RTEMS.

Inspired by Haskell's Turtle and Shelly libraries, this module provides
a functional approach to shell operations with strong typing and
composable pipelines.

## Philosophy

Like Turtle, this module treats shell commands as producers of streams
that can be folded, filtered, and transformed. The `Shell` type represents
a stream of values that can be processed lazily.

## Example

    import Rtems.Shell exposing (..)

    -- List all .c files in src/ and count lines
    countSourceLines : Shell Int
    countSourceLines =
        find (glob "*.c") (fromString "src/")
            |> select cat
            |> fold countLines

    -- Copy all log files to backup
    backupLogs : Cmd msg
    backupLogs =
        glob "*.log" (fromString "/var/log")
            |> select (\f -> cp f (fromString "/backup" </> filename f))
            |> run

-}


-- ============================================================================
-- FILE PATHS
-- ============================================================================


{-| A file path with type safety.

Unlike raw strings, FilePath ensures proper path handling across platforms.
-}
type FilePath
    = FilePath String


{-| Create a FilePath from a String.
-}
fromString : String -> FilePath
fromString s =
    FilePath s


{-| Convert a FilePath to a String.
-}
toString : FilePath -> String
toString (FilePath s) =
    s


{-| Join two paths with a separator.

    fromString "/home" </> fromString "user" == fromString "/home/user"

-}
(</>) : FilePath -> FilePath -> FilePath
(</>) (FilePath a) (FilePath b) =
    if String.endsWith "/" a then
        FilePath (a ++ b)

    else
        FilePath (a ++ "/" ++ b)


infixr 5 </>


{-| Get the parent directory.

    parent (fromString "/home/user/file.txt") == fromString "/home/user"

-}
parent : FilePath -> FilePath
parent (FilePath p) =
    case String.split "/" p |> List.reverse |> List.drop 1 |> List.reverse of
        [] ->
            FilePath "."

        parts ->
            FilePath (String.join "/" parts)


{-| Get the filename component.

    filename (fromString "/home/user/file.txt") == fromString "file.txt"

-}
filename : FilePath -> FilePath
filename (FilePath p) =
    case String.split "/" p |> List.reverse |> List.head of
        Just name ->
            FilePath name

        Nothing ->
            FilePath ""


{-| Get the file extension.

    extension (fromString "file.txt") == Just "txt"

-}
extension : FilePath -> Maybe String
extension (FilePath p) =
    case String.split "." p |> List.reverse |> List.head of
        Just ext ->
            if String.contains "/" ext then
                Nothing

            else
                Just ext

        Nothing ->
            Nothing


{-| Check if path has given extension.
-}
hasExtension : String -> FilePath -> Bool
hasExtension ext path =
    extension path == Just ext


{-| Remove the extension.
-}
dropExtension : FilePath -> FilePath
dropExtension (FilePath p) =
    case String.indices "." p |> List.reverse |> List.head of
        Just idx ->
            FilePath (String.left idx p)

        Nothing ->
            FilePath p


{-| Add an extension.
-}
addExtension : String -> FilePath -> FilePath
addExtension ext (FilePath p) =
    FilePath (p ++ "." ++ ext)


{-| Split path into directory components.
-}
splitDirectories : FilePath -> List FilePath
splitDirectories (FilePath p) =
    String.split "/" p
        |> List.filter (not << String.isEmpty)
        |> List.map FilePath


{-| Join directory components into a path.
-}
joinPath : List FilePath -> FilePath
joinPath parts =
    parts
        |> List.map toString
        |> String.join "/"
        |> FilePath



-- ============================================================================
-- SHELL MONAD
-- ============================================================================


{-| A shell computation that produces a stream of values.

Like Haskell's `Shell` from Turtle, this represents a computation that
can yield multiple values, similar to a list but with effects.
-}
type Shell a
    = Shell (() -> List a)


{-| Run a shell computation, discarding results.
-}
run : Shell a -> Cmd msg
run (Shell f) =
    -- Kernel: tcelm_shell_run
    Elm.Kernel.Shell.run f


{-| Run a shell computation with a fold.
-}
runFold : Fold a b -> Shell a -> b
runFold folder (Shell f) =
    foldList folder (f ())


{-| Create a shell with a single value.
-}
single : a -> Shell a
single x =
    Shell (\() -> [ x ])


{-| Create a shell from a list of values.
-}
select : (a -> Shell b) -> Shell a -> Shell b
select f (Shell xs) =
    Shell (\() -> List.concatMap (\x -> let (Shell g) = f x in g ()) (xs ()))


{-| Empty shell (no values).
-}
empty : Shell a
empty =
    Shell (\() -> [])


{-| Alternative: try first shell, if empty try second.
-}
(<|>) : Shell a -> Shell a -> Shell a
(<|>) (Shell f) (Shell g) =
    Shell
        (\() ->
            case f () of
                [] ->
                    g ()

                xs ->
                    xs
        )


infixl 3 <|>



-- ============================================================================
-- DIRECTORY OPERATIONS
-- ============================================================================


{-| Get current working directory.
-}
pwd : Shell FilePath
pwd =
    -- Kernel: tcelm_shell_pwd
    Shell (\() -> [ Elm.Kernel.Shell.pwd () ])


{-| Change directory.
-}
cd : FilePath -> Cmd msg
cd path =
    -- Kernel: tcelm_shell_cd
    Elm.Kernel.Shell.cd (toString path)


{-| Get home directory.
-}
home : Shell FilePath
home =
    -- Kernel: tcelm_shell_home
    Shell (\() -> [ Elm.Kernel.Shell.home () ])


{-| List directory contents.

    ls (fromString "/home/user")

-}
ls : FilePath -> Shell FilePath
ls dir =
    -- Kernel: tcelm_shell_ls
    Shell (\() -> Elm.Kernel.Shell.ls (toString dir) |> List.map FilePath)


{-| List directory tree recursively.
-}
lsTree : FilePath -> Shell FilePath
lsTree dir =
    -- Kernel: tcelm_shell_lstree
    Shell (\() -> Elm.Kernel.Shell.lstree (toString dir) |> List.map FilePath)


{-| Create directory.
-}
mkdir : FilePath -> Cmd msg
mkdir path =
    -- Kernel: tcelm_shell_mkdir
    Elm.Kernel.Shell.mkdir (toString path)


{-| Create directory tree (like mkdir -p).
-}
mkdirTree : FilePath -> Cmd msg
mkdirTree path =
    -- Kernel: tcelm_shell_mkdirtree
    Elm.Kernel.Shell.mkdirtree (toString path)


{-| Remove empty directory.
-}
rmdir : FilePath -> Cmd msg
rmdir path =
    -- Kernel: tcelm_shell_rmdir
    Elm.Kernel.Shell.rmdir (toString path)


{-| Remove directory tree recursively.
-}
rmtree : FilePath -> Cmd msg
rmtree path =
    -- Kernel: tcelm_shell_rmtree
    Elm.Kernel.Shell.rmtree (toString path)



-- ============================================================================
-- FILE OPERATIONS
-- ============================================================================


{-| Create empty file or update timestamp.
-}
touch : FilePath -> Cmd msg
touch path =
    Elm.Kernel.Shell.touch (toString path)


{-| Copy file.
-}
cp : FilePath -> FilePath -> Cmd msg
cp src dst =
    Elm.Kernel.Shell.cp (toString src) (toString dst)


{-| Move/rename file.
-}
mv : FilePath -> FilePath -> Cmd msg
mv src dst =
    Elm.Kernel.Shell.mv (toString src) (toString dst)


{-| Remove file.
-}
rm : FilePath -> Cmd msg
rm path =
    Elm.Kernel.Shell.rm (toString path)


{-| Read file contents as lines (streaming).
-}
cat : FilePath -> Shell Line
cat path =
    Shell (\() -> Elm.Kernel.Shell.cat (toString path) |> List.map Line)


{-| Read entire file as string.
-}
readFile : FilePath -> String
readFile path =
    Elm.Kernel.Shell.readFile (toString path)


{-| Write string to file (overwrite).
-}
writeFile : FilePath -> String -> Cmd msg
writeFile path content =
    Elm.Kernel.Shell.writeFile (toString path) content


{-| Append string to file.
-}
appendFile : FilePath -> String -> Cmd msg
appendFile path content =
    Elm.Kernel.Shell.appendFile (toString path) content


{-| Check if file exists.
-}
fileExists : FilePath -> Bool
fileExists path =
    Elm.Kernel.Shell.fileExists (toString path)


{-| Check if directory exists.
-}
dirExists : FilePath -> Bool
dirExists path =
    Elm.Kernel.Shell.dirExists (toString path)


{-| Check if path is a regular file.
-}
isFile : FilePath -> Bool
isFile path =
    Elm.Kernel.Shell.isFile (toString path)


{-| Check if path is a directory.
-}
isDir : FilePath -> Bool
isDir path =
    Elm.Kernel.Shell.isDir (toString path)


{-| Get file size in bytes.
-}
fileSize : FilePath -> Int
fileSize path =
    Elm.Kernel.Shell.fileSize (toString path)



-- ============================================================================
-- TEXT PROCESSING
-- ============================================================================


{-| A line of text (without newline).
-}
type Line
    = Line String


{-| Convert Line to text.
-}
lineToText : Line -> String
lineToText (Line s) =
    s


{-| Convert text to Line.
-}
textToLine : String -> Line
textToLine s =
    Line s


{-| Read lines from standard input.
-}
input : Shell Line
input =
    Shell (\() -> Elm.Kernel.Shell.input () |> List.map Line)


{-| Alias for input.
-}
stdin : Shell Line
stdin =
    input


{-| Write line to standard output.
-}
output : Line -> Cmd msg
output (Line s) =
    Elm.Kernel.Shell.output s


{-| Write to stdout.
-}
stdout : Line -> Cmd msg
stdout =
    output


{-| Write to stderr.
-}
stderr : Line -> Cmd msg
stderr (Line s) =
    Elm.Kernel.Shell.stderr s


{-| Filter lines matching pattern.

    cat (fromString "log.txt")
        |> grep "ERROR"

-}
grep : String -> Shell Line -> Shell Line
grep pattern (Shell f) =
    Shell
        (\() ->
            f ()
                |> List.filter (\(Line s) -> String.contains pattern s)
        )


{-| Replace pattern in lines (like sed s/pattern/replacement/).

    cat file |> sed "old" "new"

-}
sed : String -> String -> Shell Line -> Shell Line
sed pattern replacement (Shell f) =
    Shell
        (\() ->
            f ()
                |> List.map (\(Line s) -> Line (String.replace pattern replacement s))
        )


{-| Extract fields from lines (like cut -d' ' -f1).

    cat file |> cut " " 0  -- First space-separated field

-}
cut : String -> Int -> Shell Line -> Shell Line
cut delimiter field (Shell f) =
    Shell
        (\() ->
            f ()
                |> List.map
                    (\(Line s) ->
                        String.split delimiter s
                            |> List.drop field
                            |> List.head
                            |> Maybe.withDefault ""
                            |> Line
                    )
        )


{-| Sort lines.
-}
sort : Shell Line -> Shell Line
sort (Shell f) =
    Shell
        (\() ->
            f ()
                |> List.sortBy (\(Line s) -> s)
        )


{-| Remove consecutive duplicate lines.
-}
uniq : Shell Line -> Shell Line
uniq (Shell f) =
    Shell
        (\() ->
            f ()
                |> List.foldl
                    (\line acc ->
                        case acc of
                            [] ->
                                [ line ]

                            prev :: _ ->
                                if prev == line then
                                    acc

                                else
                                    line :: acc
                    )
                    []
                |> List.reverse
        )


{-| Take first n lines.
-}
head : Int -> Shell Line -> Shell Line
head n (Shell f) =
    Shell (\() -> f () |> List.take n)


{-| Take last n lines.
-}
tail : Int -> Shell Line -> Shell Line
tail n (Shell f) =
    Shell (\() -> f () |> List.reverse |> List.take n |> List.reverse)


{-| Count lines, words, characters.
-}
wc : Shell Line -> { lines : Int, words : Int, chars : Int }
wc (Shell f) =
    let
        lines =
            f ()

        lineCount =
            List.length lines

        wordCount =
            lines
                |> List.map (\(Line s) -> String.words s |> List.length)
                |> List.sum

        charCount =
            lines
                |> List.map (\(Line s) -> String.length s)
                |> List.sum
    in
    { lines = lineCount, words = wordCount, chars = charCount }



-- ============================================================================
-- PROCESS EXECUTION
-- ============================================================================


{-| Exit code from a process.
-}
type ExitCode
    = ExitSuccess
    | ExitFailure Int


{-| Run a process with arguments.

    proc "ls" [ "-la", "/home" ]

-}
proc : String -> List String -> Cmd msg
proc cmd args =
    Elm.Kernel.Shell.proc cmd args


{-| Run a shell command string.

    shell "ls -la | grep foo"

-}
shell : String -> Cmd msg
shell cmd =
    Elm.Kernel.Shell.shell cmd


{-| Run process and stream stdout lines.

    inproc "find" [ ".", "-name", "*.c" ] empty
        |> select cat
        |> grep "TODO"

-}
inproc : String -> List String -> Shell Line -> Shell Line
inproc cmd args (Shell input_) =
    Shell
        (\() ->
            Elm.Kernel.Shell.inproc cmd args (input_ () |> List.map lineToText)
                |> List.map Line
        )


{-| Run shell command and stream stdout lines.
-}
inshell : String -> Shell Line -> Shell Line
inshell cmd (Shell input_) =
    Shell
        (\() ->
            Elm.Kernel.Shell.inshell cmd (input_ () |> List.map lineToText)
                |> List.map Line
        )


{-| Run process and get exit code.
-}
procExit : String -> List String -> ExitCode
procExit cmd args =
    case Elm.Kernel.Shell.procExit cmd args of
        0 ->
            ExitSuccess

        n ->
            ExitFailure n


{-| Run process and get stdout as string.
-}
procStrict : String -> List String -> String
procStrict cmd args =
    Elm.Kernel.Shell.procStrict cmd args


{-| Run process and get stdout and stderr.
-}
procStrictWithErr : String -> List String -> { stdout : String, stderr : String, exit : ExitCode }
procStrictWithErr cmd args =
    let
        result =
            Elm.Kernel.Shell.procStrictWithErr cmd args
    in
    { stdout = result.stdout
    , stderr = result.stderr
    , exit =
        if result.exitCode == 0 then
            ExitSuccess

        else
            ExitFailure result.exitCode
    }



-- ============================================================================
-- PATTERNS (GLOB)
-- ============================================================================


{-| A glob pattern for matching file paths.
-}
type Pattern
    = Pattern String


{-| Check if a path matches a pattern.

    match (glob "*.txt") (fromString "file.txt") == True

-}
match : Pattern -> FilePath -> Bool
match (Pattern pat) (FilePath path) =
    Elm.Kernel.Shell.globMatch pat path


{-| Create a glob pattern.

Supports:
- `*` matches any characters except /
- `**` matches any characters including /
- `?` matches single character
- `[abc]` matches character class

-}
glob : String -> Pattern
glob =
    Pattern


{-| Find files matching a pattern in a directory.

    find (glob "*.elm") (fromString "src/")

-}
find : Pattern -> FilePath -> Shell FilePath
find (Pattern pat) dir =
    Shell
        (\() ->
            Elm.Kernel.Shell.find pat (toString dir)
                |> List.map FilePath
        )



-- ============================================================================
-- UTILITIES
-- ============================================================================


{-| Print text to stdout.
-}
echo : String -> Cmd msg
echo text =
    Elm.Kernel.Shell.echo text


{-| Formatted print (like C printf).
-}
printf : String -> List String -> Cmd msg
printf fmt args =
    Elm.Kernel.Shell.printf fmt args


{-| Get current date/time.
-}
date : () -> String
date () =
    Elm.Kernel.Shell.date ()


{-| Sleep for milliseconds.
-}
sleep : Int -> Cmd msg
sleep ms =
    Elm.Kernel.Shell.sleep ms


{-| Get environment variable.
-}
env : String -> Maybe String
env name =
    Elm.Kernel.Shell.env name


{-| Set environment variable.
-}
export : String -> String -> Cmd msg
export name value =
    Elm.Kernel.Shell.export name value


{-| Find executable in PATH.
-}
which : String -> Maybe FilePath
which name =
    Elm.Kernel.Shell.which name
        |> Maybe.map FilePath


{-| Get hostname.
-}
hostname : () -> String
hostname () =
    Elm.Kernel.Shell.hostname ()



-- ============================================================================
-- FOLDING
-- ============================================================================


{-| A fold for reducing shell output.

Like Haskell's `Fold` from the foldl package, this provides composable
reducers for stream processing.
-}
type Fold a b
    = Fold (b -> a -> b) b (b -> b)


{-| Fold over a shell stream.
-}
fold : Fold a b -> Shell a -> b
fold (Fold step init_ done) (Shell f) =
    f () |> List.foldl step init_ |> done


{-| Fold with IO effects.
-}
foldIO : Fold a b -> Shell a -> Cmd b
foldIO folder sh =
    -- Returns the fold result as a command
    Elm.Kernel.Shell.foldIO (fold folder sh)


{-| Collect all values into a list.
-}
list : Fold a (List a)
list =
    Fold (\acc x -> x :: acc) [] List.reverse


{-| Count items.
-}
countLines : Fold a Int
countLines =
    Fold (\acc _ -> acc + 1) 0 identity


{-| Get first item.
-}
head_ : Fold a (Maybe a)
head_ =
    Fold
        (\acc x ->
            case acc of
                Nothing ->
                    Just x

                just ->
                    just
        )
        Nothing
        identity


{-| Get last item.
-}
last_ : Fold a (Maybe a)
last_ =
    Fold (\_ x -> Just x) Nothing identity


{-| Helper to fold a list.
-}
foldList : Fold a b -> List a -> b
foldList (Fold step init_ done) xs =
    List.foldl step init_ xs |> done
