module Format.ElmStructure exposing
    ( spaceSepOrStack, forceableSpaceSepOrStack, forceableSpaceSepOrStack1
    , spaceSepOrIndented, spaceSepOrAligned
    , spaceSepOrPrefix, prefixOrIndented
    , equalsPair, definition
    , application, group, groupWithTrailing
    , extensionGroup
    , fitsOnLine, tryOneLine
    )

{-| ElmStructure - Layout combinators for the Elm formatter.

This module provides high-level functions for common Elm code patterns,
implementing the 94-column width-aware horizontal-first formatting strategy.

@docs spaceSepOrStack, forceableSpaceSepOrStack, forceableSpaceSepOrStack1
@docs spaceSepOrIndented, spaceSepOrAligned
@docs spaceSepOrPrefix, prefixOrIndented
@docs equalsPair, definition
@docs application, group, groupWithTrailing
@docs extensionGroup
@docs fitsOnLine, tryOneLine

-}

import Format.Box as Box exposing (Box(..), Line, allSingles, line, lineLength, punc, row, space, stack1)


{-| Check if a box fits on a line given current column and max width.
-}
fitsOnLine : Int -> Box -> Bool
fitsOnLine currentCol box =
    case box of
        SingleLine l ->
            lineLength currentCol l <= Box.maxWidth

        _ ->
            False


{-| Calculate the width of a box if it were on one line.
-}
boxWidth : Box -> Int
boxWidth box =
    case box of
        SingleLine l ->
            lineLength 0 l

        Stack l1 l2 rest ->
            -- Total width if joined with spaces
            List.map (lineLength 0) (l1 :: l2 :: rest)
                |> List.sum
                |> (+) (List.length rest + 1)

        MustBreak l ->
            lineLength 0 l


{-| Try to format as single line, return Nothing if too wide.
-}
tryOneLine : Int -> Box -> Maybe Box
tryOneLine startCol box =
    if fitsOnLine startCol box then
        Just box

    else
        Nothing



-- SPACE SEPARATED OR STACKED


{-| Same as `forceableSpaceSepOrStack False`.
-}
spaceSepOrStack : Box -> List Box -> Box
spaceSepOrStack =
    forceableSpaceSepOrStack False


{-| Format as space-separated on one line, or stack vertically.

    first rest0 rest1

    first
    rest0
    rest1

-}
forceableSpaceSepOrStack : Bool -> Box -> List Box -> Box
forceableSpaceSepOrStack forceMultiline first rest =
    case ( forceMultiline, first, allSingles rest ) of
        ( False, SingleLine first_, Ok rest_ ) ->
            let
                combined =
                    line (row (List.intersperse space (first_ :: rest_)))
            in
            if fitsOnLine 0 combined then
                combined

            else
                stack1 (first :: rest)

        _ ->
            stack1 (first :: rest)


{-| Same as `forceableSpaceSepOrStack` but takes a non-empty list.
-}
forceableSpaceSepOrStack1 : Bool -> List Box -> Box
forceableSpaceSepOrStack1 forceMultiline boxes =
    case boxes of
        first :: rest ->
            forceableSpaceSepOrStack forceMultiline first rest

        [] ->
            line (Box.literal "")



-- SPACE SEPARATED OR INDENTED


{-| Format as space-separated on one line, or indent rest.

    first rest0 rest1 rest2

    first
        rest0
        rest1
        rest2

-}
spaceSepOrIndented : Box -> List Box -> Box
spaceSepOrIndented first rest =
    case ( first, allSingles rest ) of
        ( SingleLine first_, Ok rest_ ) ->
            let
                combined =
                    line (row (List.intersperse space (first_ :: rest_)))
            in
            if fitsOnLine 0 combined then
                combined

            else
                stack1 (first :: List.map Box.indent rest)

        _ ->
            stack1 (first :: List.map Box.indent rest)


{-| Format as space-separated on one line, or align continuations.

    first rest0 rest1 rest2

    first rest0 rest1
          rest2 rest3

This aligns continuations with the first argument.

-}
spaceSepOrAligned : Box -> List Box -> Box
spaceSepOrAligned first rest =
    case ( first, allSingles rest ) of
        ( SingleLine first_, Ok rest_ ) ->
            let
                combined =
                    line (row (List.intersperse space (first_ :: rest_)))
            in
            if fitsOnLine 0 combined then
                combined

            else
                -- Try to fit as many as possible on first line
                let
                    firstWidth =
                        lineLength 0 first_ + 1

                    -- +1 for space
                    alignCol =
                        firstWidth

                    tryFit : List Line -> List Line -> List Box -> ( List Line, List Box )
                    tryFit currentLine acc remaining =
                        case remaining of
                            [] ->
                                ( List.reverse (row (List.reverse currentLine) :: acc), [] )

                            x :: xs ->
                                case x of
                                    SingleLine l ->
                                        let
                                            currentWidth =
                                                lineLength 0 (row (List.reverse currentLine))

                                            addedWidth =
                                                1 + lineLength 0 l
                                        in
                                        if currentWidth + addedWidth <= Box.maxWidth then
                                            tryFit (l :: space :: currentLine) acc xs

                                        else
                                            -- Start new line
                                            let
                                                finishedLine =
                                                    row (List.reverse currentLine)
                                            in
                                            tryFit [ l ] (finishedLine :: acc) xs

                                    _ ->
                                        ( List.reverse (row (List.reverse currentLine) :: acc), x :: xs )

                    ( fittedLines, overflowBoxes ) =
                        tryFit [ first_ ] [] rest
                in
                case ( fittedLines, overflowBoxes ) of
                    ( [ singleLine ], [] ) ->
                        line singleLine

                    ( firstLine :: restLines, [] ) ->
                        let
                            alignedRest =
                                List.map (\l -> line (row [ Box.literal (String.repeat alignCol " "), l ])) restLines
                        in
                        stack1 (line firstLine :: alignedRest)

                    _ ->
                        -- Fallback to indented
                        stack1 (first :: List.map Box.indent rest)

        _ ->
            stack1 (first :: List.map Box.indent rest)



-- PREFIX OR INDENTED


{-| Format with operator prefix on same line or indented.

    op rest

    op rest1
       rest2

    opLong
        rest

-}
spaceSepOrPrefix : Box -> Box -> Box
spaceSepOrPrefix op rest =
    case ( op, rest ) of
        ( SingleLine op_, SingleLine rest_ ) ->
            let
                combined =
                    line (row [ op_, space, rest_ ])
            in
            if fitsOnLine 0 combined then
                combined

            else if lineLength 0 op_ < 4 then
                Box.prefix (row [ op_, space ]) rest

            else
                stack1 [ op, Box.indent rest ]

        ( SingleLine op_, _ ) ->
            if lineLength 0 op_ < 4 then
                Box.prefix (row [ op_, space ]) rest

            else
                stack1 [ op, Box.indent rest ]

        _ ->
            stack1 [ op, Box.indent rest ]


{-| Format with prefix or indented.
-}
prefixOrIndented : Box -> Box -> Box
prefixOrIndented a b =
    case ( a, b ) of
        ( SingleLine a_, SingleLine b_ ) ->
            let
                combined =
                    line (row [ a_, space, b_ ])
            in
            if fitsOnLine 0 combined then
                combined

            else
                stack1 [ a, Box.indent b ]

        ( SingleLine a_, MustBreak b_ ) ->
            Box.mustBreak (row [ a_, space, b_ ])

        _ ->
            stack1 [ a, Box.indent b ]



-- EQUALS PAIR


{-| Format an equals pair (used for record fields, let bindings).

    left = right

    left =
        right

    left
        =
        right

-}
equalsPair : String -> Bool -> Box -> Box -> Box
equalsPair symbol forceMultiline left right =
    case ( forceMultiline, left, right ) of
        ( False, SingleLine left_, SingleLine right_ ) ->
            let
                combined =
                    line
                        (row
                            [ left_
                            , space
                            , punc symbol
                            , space
                            , right_
                            ]
                        )
            in
            if fitsOnLine 0 combined then
                combined

            else
                stack1
                    [ line (row [ left_, space, punc symbol ])
                    , Box.indent right
                    ]

        ( _, SingleLine left_, MustBreak right_ ) ->
            Box.mustBreak
                (row
                    [ left_
                    , space
                    , punc symbol
                    , space
                    , right_
                    ]
                )

        ( _, SingleLine left_, _ ) ->
            stack1
                [ line (row [ left_, space, punc symbol ])
                , Box.indent right
                ]

        _ ->
            stack1
                [ left
                , Box.indent (line (punc symbol))
                , Box.indent right
                ]


{-| An equalsPair where the left side is an application.
-}
definition : String -> Bool -> Box -> List Box -> Box -> Box
definition symbol forceMultiline first rest body =
    equalsPair symbol forceMultiline (application first rest) body



-- APPLICATION


{-| Format a function application.

    first rest0 rest1 rest2

    first rest0
          rest1
          rest2

    first
        rest0
        rest1
        rest2

-}
application : Box -> List Box -> Box
application first args =
    case args of
        [] ->
            first

        _ ->
            spaceSepOrAligned first args



-- GROUP


{-| Format a grouped structure like lists, records, tuples.

    <>

    < child0 >

    < child0, child1, child2 >

    < child0
    , child1
    , child2 >

K&R style: closing delimiter on same line as last element.

-}
group : Bool -> String -> String -> String -> Bool -> List Box -> Box
group innerSpaces left sep right forceMultiline children =
    groupWithTrailing innerSpaces left sep right forceMultiline children []


{-| Group with trailing content before closing delimiter.
-}
groupWithTrailing : Bool -> String -> String -> String -> Bool -> List Box -> List Box -> Box
groupWithTrailing innerSpaces left sep right forceMultiline children trailing =
    case ( allSingles children, allSingles trailing ) of
        ( Ok [], Ok ts ) ->
            -- Empty group
            line (row ([ punc left ] ++ ts ++ [ punc right ]))

        ( Ok ls, Ok ts ) ->
            if forceMultiline then
                groupMultiline left sep right ls ts

            else
                let
                    innerContent =
                        List.intersperse (row [ punc sep, space ]) (ls ++ ts)

                    combined =
                        if innerSpaces then
                            line (row ([ punc left, space ] ++ innerContent ++ [ space, punc right ]))

                        else
                            line (row ([ punc left ] ++ innerContent ++ [ punc right ]))
                in
                if fitsOnLine 0 combined then
                    combined

                else
                    groupMultiline left sep right ls ts

        _ ->
            case children of
                [] ->
                    line (row [ punc left, punc right ])

                first :: rest ->
                    groupMultilineBoxes left sep right first rest trailing


{-| Format a group across multiple lines with K&R closing.
-}
groupMultiline : String -> String -> String -> List Line -> List Line -> Box
groupMultiline left sep right children trailing =
    case children of
        [] ->
            line (row [ punc left, punc right ])

        [ single ] ->
            -- Single item with K&R closing
            line (row [ punc left, space, single, space, punc right ])

        first :: rest ->
            let
                firstLine =
                    line (row [ punc left, space, first ])

                middleLines =
                    List.map (\l -> line (row [ punc sep, space, l ])) (List.take (List.length rest - 1) rest)

                trailingBoxes =
                    List.map line trailing

                lastContent =
                    List.drop (List.length rest - 1) rest
                        |> List.head

                lastLine =
                    case lastContent of
                        Just l ->
                            line (row [ punc sep, space, l, space, punc right ])

                        Nothing ->
                            line (punc right)
            in
            stack1 (firstLine :: middleLines ++ trailingBoxes ++ [ lastLine ])


{-| Format a group of boxes across multiple lines.
-}
groupMultilineBoxes : String -> String -> String -> Box -> List Box -> List Box -> Box
groupMultilineBoxes left sep right first rest trailing =
    let
        firstBox =
            Box.prefix (row [ punc left, space ]) first

        middleBoxes =
            List.map (Box.prefix (row [ punc sep, space ])) rest

        closingLine =
            line (punc right)
    in
    stack1 (firstBox :: middleBoxes ++ trailing ++ [ closingLine ])



-- EXTENSION GROUP (Records with base)


{-| Format a record extension.

    { base | first }

    { base | first, rest0, rest1 }

    { base
        | first
        , rest0
        , rest1
    }

-}
extensionGroup : Bool -> Box -> Box -> List Box -> Box
extensionGroup multiline base first rest =
    case ( multiline, base, allSingles (first :: rest) ) of
        ( False, SingleLine base_, Ok fields_ ) ->
            let
                fieldContent =
                    List.intersperse (row [ punc ",", space ]) fields_

                combined =
                    line
                        (row
                            [ punc "{"
                            , space
                            , base_
                            , space
                            , punc "|"
                            , space
                            , row fieldContent
                            , space
                            , punc "}"
                            ]
                        )
            in
            if fitsOnLine 0 combined then
                combined

            else
                extensionGroupMultiline base first rest

        _ ->
            extensionGroupMultiline base first rest


{-| Format extension group across multiple lines.
-}
extensionGroupMultiline : Box -> Box -> List Box -> Box
extensionGroupMultiline base first rest =
    stack1
        [ Box.prefix (row [ punc "{", space ]) base
        , stack1
            (Box.prefix (row [ punc "|", space ]) first
                :: List.map (Box.prefix (row [ punc ",", space ])) rest
            )
            |> Box.indent
        , line (punc "}")
        ]
