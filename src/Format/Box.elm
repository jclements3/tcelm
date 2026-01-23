module Format.Box exposing
    ( Line, identifier, keyword, punc, literal, row, space
    , Box(..), blankLine, line, mustBreak, stack, stack1, andThen
    , isLine, allSingles, lineLength
    , indent, prefix, addSuffix, alignTo
    , render, renderWithWidth
    , maxWidth
    )

{-| Box - Core rendering types for the Elm formatter.

This module provides the fundamental data structures for representing
formatted code as lines and boxes that can be combined and rendered.

@docs Line, identifier, keyword, punc, literal, row, space
@docs Box, blankLine, line, mustBreak, stack, stack1, andThen
@docs isLine, allSingles, lineLength
@docs indent, prefix, addSuffix, alignTo
@docs render, renderWithWidth
@docs maxWidth

-}


{-| Maximum line width for formatting (94 columns).
-}
maxWidth : Int
maxWidth =
    94


{-| Number of spaces per indentation level.
-}
spacesInTab : Int
spacesInTab =
    4



-- LINE


{-| A Line is always exactly one line of output.
-}
type Line
    = Text String
    | Row (List Line)
    | Space
    | Tab


{-| Create an identifier (variable name, etc).
-}
identifier : String -> Line
identifier =
    Text


{-| Create a keyword.
-}
keyword : String -> Line
keyword =
    Text


{-| Create punctuation.
-}
punc : String -> Line
punc =
    Text


{-| Create a literal value.
-}
literal : String -> Line
literal =
    Text


{-| Join multiple Line elements into one.
-}
row : List Line -> Line
row =
    Row


{-| A single space.
-}
space : Line
space =
    Space



-- BOX


{-| Box contains one or more Lines.

  - SingleLine: Can appear in the middle of a line
  - Stack: Multiple lines (first line may get special treatment)
  - MustBreak: Must appear on its own line (for -- comments)

-}
type Box
    = SingleLine Line
    | Stack Line Line (List Line)
    | MustBreak Line


{-| A blank line.
-}
blankLine : Box
blankLine =
    line (literal "")


{-| Create a single-line box.
-}
line : Line -> Box
line l =
    SingleLine l


{-| Create a box that must break (for line comments).
-}
mustBreak : Line -> Box
mustBreak l =
    MustBreak l


{-| Stack two boxes vertically.
-}
stack : Box -> Box -> Box
stack b1 b2 =
    let
        ( line1first, line1rest ) =
            destructure b1

        ( line2first, line2rest ) =
            destructure b2
    in
    case line1rest ++ (line2first :: line2rest) of
        [] ->
            -- This should never happen since line2first is always present
            SingleLine line1first

        first :: rest ->
            Stack line1first first rest


{-| Chain multiple boxes after a first one.
-}
andThen : List Box -> Box -> Box
andThen rest first =
    List.foldl stack first rest


{-| Stack a non-empty list of boxes.
-}
stack1 : List Box -> Box
stack1 children =
    case children of
        [] ->
            -- Error case - should not happen with valid input
            line (literal "")

        [ single ] ->
            single

        first :: rest ->
            List.foldl (\b acc -> stack acc b) first rest


{-| Apply a function to all lines in a box.
-}
mapLines : (Line -> Line) -> Box -> Box
mapLines fn =
    mapFirstLine fn fn


{-| Apply different functions to first line vs rest.
-}
mapFirstLine : (Line -> Line) -> (Line -> Line) -> Box -> Box
mapFirstLine firstFn restFn b =
    case b of
        SingleLine l1 ->
            SingleLine (firstFn l1)

        Stack l1 l2 ls ->
            Stack (firstFn l1) (restFn l2) (List.map restFn ls)

        MustBreak l1 ->
            MustBreak (firstFn l1)


{-| Indent a box by one tab stop.
-}
indent : Box -> Box
indent =
    mapLines (\l -> row [ Tab, l ])


{-| Align continuation lines to a specific column.
-}
alignTo : Int -> Box -> Box
alignTo col =
    mapFirstLine identity (\l -> row [ Text (String.repeat col " "), l ])


{-| Check if a box is a single line, returning the line or the box.
-}
isLine : Box -> Result Box Line
isLine b =
    case b of
        SingleLine l ->
            Ok l

        _ ->
            Err b


{-| Destructure a box into its first line and remaining lines.
-}
destructure : Box -> ( Line, List Line )
destructure b =
    case b of
        SingleLine l1 ->
            ( l1, [] )

        Stack l1 l2 rest ->
            ( l1, l2 :: rest )

        MustBreak l1 ->
            ( l1, [] )


{-| Check if all boxes are single lines.
-}
allSingles : List Box -> Result (List Box) (List Line)
allSingles boxes =
    let
        toLines acc remaining =
            case remaining of
                [] ->
                    Ok (List.reverse acc)

                b :: rest ->
                    case isLine b of
                        Ok l ->
                            toLines (l :: acc) rest

                        Err _ ->
                            Err boxes
    in
    toLines [] boxes


{-| Add a prefix to the first line, pad other lines with spaces.
-}
prefix : Line -> Box -> Box
prefix pref box =
    let
        prefixLen =
            lineLength 0 pref

        paddingSpaces =
            Text (String.repeat prefixLen " ")

        padLineWithSpaces l =
            row [ paddingSpaces, l ]

        addPrefixToLine l =
            row [ pref, l ]
    in
    mapFirstLine addPrefixToLine padLineWithSpaces box


{-| Add a suffix to the last line of a box.
-}
addSuffix : Line -> Box -> Box
addSuffix suffix b =
    case destructure b of
        ( l, [] ) ->
            line (row [ l, suffix ])

        ( l1, ls ) ->
            let
                initLines =
                    List.take (List.length ls - 1) ls

                lastLine =
                    List.drop (List.length ls - 1) ls
                        |> List.head
                        |> Maybe.withDefault l1
            in
            line l1
                |> andThen (List.map line initLines)
                |> andThen [ line (row [ lastLine, suffix ]) ]



-- RENDERING


{-| Calculate the length of a line starting at a given column.
-}
lineLength : Int -> Line -> Int
lineLength startColumn l =
    case l of
        Text str ->
            startColumn + String.length str

        Space ->
            startColumn + 1

        Tab ->
            startColumn + tabLength startColumn

        Row lines ->
            List.foldl (\ln col -> lineLength col ln) startColumn lines


{-| Calculate spaces to next tab stop.
-}
tabLength : Int -> Int
tabLength startColumn =
    spacesInTab - modBy spacesInTab startColumn


{-| Render a line to a string.
-}
renderLine : Int -> Line -> String
renderLine startColumn l =
    case l of
        Text text ->
            text

        Space ->
            " "

        Tab ->
            String.repeat (tabLength startColumn) " "

        Row lines ->
            renderRow startColumn lines


{-| Render a row of lines to a string.
-}
renderRow : Int -> List Line -> String
renderRow startColumn lines =
    let
        step ln ( str, col ) =
            let
                rendered =
                    renderLine col ln

                newCol =
                    lineLength col ln
            in
            ( str ++ rendered, newCol )
    in
    List.foldl step ( "", startColumn ) lines
        |> Tuple.first


{-| Render a box to a string.
-}
render : Box -> String
render box =
    case box of
        SingleLine l ->
            String.trimRight (renderLine 0 l) ++ "\n"

        Stack l1 l2 rest ->
            (l1 :: l2 :: rest)
                |> List.map (String.trimRight << renderLine 0)
                |> String.join "\n"
                |> (\s -> s ++ "\n")

        MustBreak l ->
            String.trimRight (renderLine 0 l) ++ "\n"


{-| Render a box, returning total width information.
-}
renderWithWidth : Box -> { content : String, maxLineWidth : Int }
renderWithWidth box =
    let
        content =
            render box

        maxLineWidth =
            content
                |> String.lines
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 0
    in
    { content = content, maxLineWidth = maxLineWidth }
