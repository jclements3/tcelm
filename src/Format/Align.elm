module Format.Align exposing
    ( alignFields, alignVariants, alignCaseBranches
    , alignedRecord, alignedRecordType
    , AlignedField, makeAlignedField
    )

{-| Align - Operator alignment utilities for the Elm formatter.

This module provides functions to align operators vertically within blocks,
creating visually aligned columns for `:`, `=`, `->`, and `|` operators.

@docs alignFields, alignVariants, alignCaseBranches
@docs alignedRecord, alignedRecordType
@docs AlignedField, makeAlignedField

-}

import Format.Box as Box exposing (Box(..), Line, line, punc, row, space)
import Format.ElmStructure as ES


{-| An aligned field with computed padding.
-}
type alias AlignedField =
    { name : String
    , nameWidth : Int
    , value : Box
    }


{-| Create an aligned field.
-}
makeAlignedField : String -> Box -> AlignedField
makeAlignedField name value =
    { name = name
    , nameWidth = String.length name
    , value = value
    }


{-| Align a list of name-value pairs with a separator.

For example, with separator "=" and fields:

    [ ("host", "localhost"), ("port", "8080"), ("debug", "True") ]

Produces:

    host  = "localhost"
    port  = 8080
    debug = True

-}
alignFields : String -> List AlignedField -> List Box
alignFields separator fields =
    let
        maxNameWidth =
            fields
                |> List.map .nameWidth
                |> List.maximum
                |> Maybe.withDefault 0

        alignField field =
            let
                padding =
                    String.repeat (maxNameWidth - field.nameWidth) " "

                paddedName =
                    row [ Box.identifier field.name, Box.literal padding ]
            in
            case field.value of
                SingleLine valueLine ->
                    line (row [ paddedName, space, punc separator, space, valueLine ])

                _ ->
                    Box.stack1
                        [ line (row [ paddedName, space, punc separator ])
                        , Box.indent field.value
                        ]
    in
    List.map alignField fields


{-| Align union type variants.

For example:

    type Msg
        = Click
        | Hover     String
        | KeyPress  Char
        | MouseMove Int Int

-}
alignVariants : List ( String, List Box ) -> List Box
alignVariants variants =
    let
        maxNameWidth =
            variants
                |> List.map (\( name, _ ) -> String.length name)
                |> List.maximum
                |> Maybe.withDefault 0

        alignVariant ( name, args ) =
            let
                padding =
                    String.repeat (maxNameWidth - String.length name) " "

                paddedName =
                    row [ Box.identifier name, Box.literal padding ]
            in
            case ( args, Box.allSingles args ) of
                ( [], _ ) ->
                    line (Box.identifier name)

                ( _, Ok argLines ) ->
                    line (row (paddedName :: space :: List.intersperse space argLines))

                _ ->
                    ES.application (line paddedName) args
    in
    List.map alignVariant variants


{-| Align case branches with `->`.

For example:

    case msg of
        Click         -> handleClick
        Hover str     -> handleHover str
        KeyPress char -> handleKeyPress char

-}
alignCaseBranches : List ( Box, Box ) -> List Box
alignCaseBranches branches =
    let
        patternWidth box =
            case box of
                SingleLine l ->
                    Box.lineLength 0 l

                _ ->
                    0

        maxPatternWidth =
            branches
                |> List.map (\( p, _ ) -> patternWidth p)
                |> List.maximum
                |> Maybe.withDefault 0

        alignBranch ( pattern, body ) =
            case ( pattern, body ) of
                ( SingleLine patternLine, SingleLine bodyLine ) ->
                    let
                        currentWidth =
                            Box.lineLength 0 patternLine

                        padding =
                            String.repeat (maxPatternWidth - currentWidth) " "

                        combined =
                            line (row [ patternLine, Box.literal padding, space, punc "->", space, bodyLine ])
                    in
                    if ES.fitsOnLine 0 combined then
                        combined

                    else
                        Box.stack1
                            [ line (row [ patternLine, space, punc "->" ])
                            , Box.indent body
                            ]

                ( SingleLine patternLine, _ ) ->
                    Box.stack1
                        [ line (row [ patternLine, space, punc "->" ])
                        , Box.indent body
                        ]

                _ ->
                    Box.stack1
                        [ pattern
                        , Box.indent (line (punc "->"))
                        , Box.indent (Box.indent body)
                        ]
    in
    List.map alignBranch branches


{-| Format a record with aligned field separators.

For short records (no alignment):

    { firstName = "John", lastName = "Doe", age = 30 }

For longer records with aligned fields and K&R closing:

    { host     = "localhost"
    , port     = 8080
    , debug    = True
    , logLevel = "info" }

-}
alignedRecord : String -> List AlignedField -> Box
alignedRecord separator fields =
    case fields of
        [] ->
            line (punc "{}")

        _ ->
            let
                -- First try unaligned single line (for short records)
                simpleFields =
                    List.map
                        (\f ->
                            case f.value of
                                SingleLine valueLine ->
                                    Just (row [ Box.identifier f.name, space, punc separator, space, valueLine ])

                                _ ->
                                    Nothing
                        )
                        fields

                trySimpleLine =
                    case sequenceMaybes simpleFields of
                        Just fieldLines ->
                            let
                                content =
                                    List.intersperse (row [ punc ",", space ]) fieldLines

                                combined =
                                    line (row ([ punc "{", space ] ++ content ++ [ space, punc "}" ]))
                            in
                            if ES.fitsOnLine 0 combined then
                                Just combined

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            case trySimpleLine of
                Just oneLine ->
                    oneLine

                Nothing ->
                    -- Multi-line with aligned fields and K&R closing
                    let
                        aligned =
                            alignFields separator fields
                    in
                    formatMultilineRecord "{" "," "}" aligned


{-| Helper to sequence a list of Maybes.
-}
sequenceMaybes : List (Maybe a) -> Maybe (List a)
sequenceMaybes list =
    List.foldr
        (\ma acc ->
            case ( ma, acc ) of
                ( Just a, Just xs ) ->
                    Just (a :: xs)

                _ ->
                    Nothing
        )
        (Just [])
        list


{-| Format a record type with aligned field separators.

For short record types (no alignment):

    { firstName : String, lastName : String, age : Int }

For longer record types with aligned fields:

    { firstName : String
    , lastName  : String
    , age       : Int }

-}
alignedRecordType : List AlignedField -> Maybe String -> Box
alignedRecordType fields maybeExt =
    case ( fields, maybeExt ) of
        ( [], Nothing ) ->
            line (punc "{}")

        ( [], Just ext ) ->
            line (row [ punc "{", space, Box.identifier ext, space, punc "}" ])

        ( _, Nothing ) ->
            let
                -- First try unaligned single line
                simpleFields =
                    List.map
                        (\f ->
                            case f.value of
                                SingleLine valueLine ->
                                    Just (row [ Box.identifier f.name, space, punc ":", space, valueLine ])

                                _ ->
                                    Nothing
                        )
                        fields

                trySimpleLine =
                    case sequenceMaybes simpleFields of
                        Just fieldLines ->
                            let
                                content =
                                    List.intersperse (row [ punc ",", space ]) fieldLines

                                combined =
                                    line (row ([ punc "{", space ] ++ content ++ [ space, punc "}" ]))
                            in
                            if ES.fitsOnLine 0 combined then
                                Just combined

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            case trySimpleLine of
                Just oneLine ->
                    oneLine

                Nothing ->
                    let
                        aligned =
                            alignFields ":" fields
                    in
                    formatMultilineRecord "{" "," "}" aligned

        ( _, Just ext ) ->
            let
                aligned =
                    alignFields ":" fields

                -- Try single line first
                tryOneLine =
                    case Box.allSingles aligned of
                        Ok lines ->
                            let
                                content =
                                    List.intersperse (row [ punc ",", space ]) lines

                                combined =
                                    line
                                        (row
                                            [ punc "{"
                                            , space
                                            , Box.identifier ext
                                            , space
                                            , punc "|"
                                            , space
                                            , row content
                                            , space
                                            , punc "}"
                                            ]
                                        )
                            in
                            if ES.fitsOnLine 0 combined then
                                Just combined

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            case tryOneLine of
                Just oneLine ->
                    oneLine

                Nothing ->
                    formatExtensionRecord ext aligned


{-| Format a multiline record with K&R closing brace.
-}
formatMultilineRecord : String -> String -> String -> List Box -> Box
formatMultilineRecord left sep right boxes =
    case boxes of
        [] ->
            line (row [ punc left, punc right ])

        [ single ] ->
            -- Single item - try to keep on one line with K&R
            case single of
                SingleLine singleLine ->
                    line (row [ punc left, space, singleLine, space, punc right ])

                _ ->
                    Box.stack1
                        [ Box.prefix (row [ punc left, space ]) single
                        , line (punc right)
                        ]

        first :: rest ->
            let
                firstLine =
                    Box.prefix (row [ punc left, space ]) first

                middleLines =
                    List.take (List.length rest - 1) rest
                        |> List.map (Box.prefix (row [ punc sep, space ]))

                lastBox =
                    List.drop (List.length rest - 1) rest
                        |> List.head
            in
            case lastBox of
                Just (SingleLine lastLine) ->
                    -- K&R: closing on same line as last element
                    let
                        lastWithClose =
                            line (row [ punc sep, space, lastLine, space, punc right ])
                    in
                    Box.stack1 (firstLine :: middleLines ++ [ lastWithClose ])

                Just lastMulti ->
                    -- Last element is multiline, put closing on its own line
                    Box.stack1 (firstLine :: middleLines ++ [ Box.prefix (row [ punc sep, space ]) lastMulti, line (punc right) ])

                Nothing ->
                    -- Only one element
                    Box.stack1 [ firstLine, line (punc right) ]


{-| Format extension record with K&R style.
-}
formatExtensionRecord : String -> List Box -> Box
formatExtensionRecord ext boxes =
    case boxes of
        [] ->
            line (row [ punc "{", space, Box.identifier ext, space, punc "}" ])

        first :: rest ->
            let
                header =
                    line (row [ punc "{", space, Box.identifier ext ])

                firstField =
                    Box.prefix (row [ punc "|", space ]) first

                middleFields =
                    List.take (List.length rest - 1) rest
                        |> List.map (Box.prefix (row [ punc ",", space ]))

                lastBox =
                    List.drop (List.length rest - 1) rest
                        |> List.head
            in
            case lastBox of
                Just (SingleLine lastLine) ->
                    let
                        lastWithClose =
                            line (row [ punc ",", space, lastLine, space, punc "}" ])
                    in
                    Box.stack1 (header :: Box.indent firstField :: List.map Box.indent middleFields ++ [ Box.indent lastWithClose ])

                Just lastMulti ->
                    Box.stack1 (header :: Box.indent firstField :: List.map Box.indent middleFields ++ [ Box.indent (Box.prefix (row [ punc ",", space ]) lastMulti), line (punc "}") ])

                Nothing ->
                    -- Only one element
                    case first of
                        SingleLine firstLine ->
                            Box.stack1
                                [ header
                                , line (row [ punc "|", space, firstLine, space, punc "}" ])
                                    |> Box.indent
                                ]

                        _ ->
                            Box.stack1
                                [ header
                                , Box.indent firstField
                                , line (punc "}")
                                ]
