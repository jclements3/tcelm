module Parse.Module exposing (parse)

{-| Module parsing for Elm.
This is the top-level parser that parses an entire Elm module.
-}

import AST.Source as Src exposing (Exposed(..), Exposing(..), Module, Position, Privacy(..))
import Parse.Declaration as Decl
import Parse.Keyword as Keyword
import Parse.Primitives as P exposing (Parser)
import Parse.Space as Space
import Parse.Symbol as Symbol
import Parse.Variable as Var
import Reporting.Error.Syntax as E



-- PARSE


parse : String -> Result (List (P.DeadEnd E.Problem)) Module
parse source =
    P.run module_ source



-- MODULE


module_ : Parser E.Problem Module
module_ =
    chompHeader
        |> P.andThen
            (\maybeHeader ->
                -- Skip optional module doc comment between header and imports
                chompModuleDocComment
                    |> P.andThen
                        (\maybeModuleDoc ->
                            chompImports []
                                |> P.andThen
                                    (\imports ->
                                        chompDecls []
                                            |> P.andThen
                                                (\decls ->
                                                    let
                                                        categorized =
                                                            categorizeDecls [] [] [] [] [] decls
                                                    in
                                                    case maybeHeader of
                                                        Just ( name, exports ) ->
                                                            P.succeed
                                                                { name = Just name
                                                                , exports = exports
                                                                , docs = maybeModuleDoc
                                                                , imports = imports
                                                                , values = categorized.values
                                                                , unions = categorized.unions
                                                                , aliases = categorized.aliases
                                                                , binops = categorized.binops
                                                                , ports = categorized.ports
                                                                , classes = []
                                                                , instances = []
                                                                , foreigns = []
                                                                }

                                                        Nothing ->
                                                            P.succeed
                                                                { name = Nothing
                                                                , exports = Src.At (Src.Region { row = 1, col = 1 } { row = 1, col = 1 }) Src.Open
                                                                , docs = maybeModuleDoc
                                                                , imports = imports
                                                                , values = categorized.values
                                                                , unions = categorized.unions
                                                                , aliases = categorized.aliases
                                                                , binops = categorized.binops
                                                                , ports = categorized.ports
                                                                , classes = []
                                                                , instances = []
                                                                , foreigns = []
                                                                }
                                                )
                                    )
                        )
            )


type alias CategorizedDecls =
    { values : List (Src.Located Src.Value)
    , unions : List (Src.Located Src.Union)
    , aliases : List (Src.Located Src.Alias)
    , binops : List (Src.Located Src.Infix)
    , ports : List Src.Port
    }


categorizeDecls :
    List (Src.Located Src.Value)
    -> List (Src.Located Src.Union)
    -> List (Src.Located Src.Alias)
    -> List (Src.Located Src.Infix)
    -> List Src.Port
    -> List Decl.Decl
    -> CategorizedDecls
categorizeDecls values unions aliases binops ports decls =
    case decls of
        [] ->
            { values = List.reverse values
            , unions = List.reverse unions
            , aliases = List.reverse aliases
            , binops = List.reverse binops
            , ports = List.reverse ports
            }

        decl :: rest ->
            case decl of
                Decl.Value value ->
                    categorizeDecls (value :: values) unions aliases binops ports rest

                Decl.Union union ->
                    categorizeDecls values (union :: unions) aliases binops ports rest

                Decl.Alias alias_ ->
                    categorizeDecls values unions (alias_ :: aliases) binops ports rest

                Decl.PortDecl port_ ->
                    categorizeDecls values unions aliases binops (port_ :: ports) rest



-- MODULE DOC COMMENT


chompModuleDocComment : Parser E.Problem (Maybe Src.DocComment)
chompModuleDocComment =
    P.oneOfWithFallback
        [ Space.chompDocComment E.ModuleSpace
        ]
        Nothing



-- HEADER


chompHeader : Parser E.Problem (Maybe ( Src.Located String, Src.Located Exposing ))
chompHeader =
    freshLine E.FreshLine
        |> P.andThen
            (\_ ->
                P.oneOfWithFallback
                    [ -- module Name exposing (..)
                      Keyword.module_ E.ModuleProblem
                        |> P.andThen
                            (\_ ->
                                Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                    |> P.andThen (\_ -> P.addLocation (Var.moduleName E.ModuleName))
                                    |> P.andThen
                                        (\name ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                                |> P.andThen (\_ -> Keyword.exposing_ E.ModuleProblem)
                                                |> P.andThen
                                                    (\_ ->
                                                        Space.chompAndCheckIndent E.ModuleSpace E.ModuleProblem
                                                            |> P.andThen (\_ -> P.addLocation exposing_)
                                                            |> P.andThen
                                                                (\exports ->
                                                                    freshLine E.FreshLine
                                                                        |> P.andThen (\_ -> P.succeed (Just ( name, exports )))
                                                                )
                                                    )
                                        )
                            )
                    , -- port module Name exposing (..)
                      Keyword.port_ E.PortModuleProblem
                        |> P.andThen
                            (\_ ->
                                Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                    |> P.andThen (\_ -> Keyword.module_ E.PortModuleProblem)
                                    |> P.andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                |> P.andThen (\_ -> P.addLocation (Var.moduleName E.PortModuleName))
                                                |> P.andThen
                                                    (\name ->
                                                        Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                            |> P.andThen (\_ -> Keyword.exposing_ E.PortModuleProblem)
                                                            |> P.andThen
                                                                (\_ ->
                                                                    Space.chompAndCheckIndent E.ModuleSpace E.PortModuleProblem
                                                                        |> P.andThen (\_ -> P.addLocation exposing_)
                                                                        |> P.andThen
                                                                            (\exports ->
                                                                                freshLine E.FreshLine
                                                                                    |> P.andThen (\_ -> P.succeed (Just ( name, exports )))
                                                                            )
                                                                )
                                                    )
                                        )
                            )
                    ]
                    Nothing
            )



-- IMPORTS


chompImports : List Src.Import -> Parser E.Problem (List Src.Import)
chompImports imports =
    P.oneOfWithFallback
        [ chompImport
            |> P.andThen (\import_ -> chompImports (import_ :: imports))
        ]
        (List.reverse imports)


chompImport : Parser E.Problem Src.Import
chompImport =
    Keyword.import_ E.ImportStart
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentName
                    |> P.andThen (\_ -> P.addLocation (Var.moduleName E.ImportName))
                    |> P.andThen
                        (\name ->
                            Space.chomp E.ModuleSpace
                                |> P.andThen
                                    (\_ ->
                                        P.oneOf E.ImportEnd
                                            [ freshLine E.ImportEnd
                                                |> P.andThen
                                                    (\_ ->
                                                        P.succeed
                                                            { name = name
                                                            , alias_ = Nothing
                                                            , exposing_ = Explicit []
                                                            }
                                                    )
                                            , chompAs name
                                            , chompExposing name Nothing
                                            ]
                                    )
                        )
            )


chompAs : Src.Located String -> Parser E.Problem Src.Import
chompAs name =
    Keyword.as_ E.ImportAs
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentAlias
                    |> P.andThen (\_ -> Var.upper E.ImportAlias)
                    |> P.andThen
                        (\alias_ ->
                            Space.chomp E.ModuleSpace
                                |> P.andThen
                                    (\_ ->
                                        P.oneOf E.ImportEnd
                                            [ freshLine E.ImportEnd
                                                |> P.andThen
                                                    (\_ ->
                                                        P.succeed
                                                            { name = name
                                                            , alias_ = Just alias_
                                                            , exposing_ = Explicit []
                                                            }
                                                    )
                                            , chompExposing name (Just alias_)
                                            ]
                                    )
                        )
            )


chompExposing : Src.Located String -> Maybe String -> Parser E.Problem Src.Import
chompExposing name maybeAlias =
    Keyword.exposing_ E.ImportExposing
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.ModuleSpace E.ImportIndentExposingList
                    |> P.andThen (\_ -> exposing_)
                    |> P.andThen
                        (\exposed ->
                            freshLine E.ImportEnd
                                |> P.andThen
                                    (\_ ->
                                        P.succeed
                                            { name = name
                                            , alias_ = maybeAlias
                                            , exposing_ = exposed
                                            }
                                    )
                        )
            )



-- EXPOSING


exposing_ : Parser E.Problem Exposing
exposing_ =
    P.symbol "(" E.ExposingStart
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
                    |> P.andThen
                        (\_ ->
                            P.oneOf E.ExposingValue
                                [ P.symbol ".." E.ExposingValue
                                    |> P.andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.andThen (\_ -> P.symbol ")" E.ExposingEnd)
                                                |> P.andThen (\_ -> P.succeed Open)
                                        )
                                , chompExposed
                                    |> P.andThen
                                        (\exposed ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.andThen (\_ -> exposingHelp [ exposed ])
                                        )
                                ]
                        )
            )


exposingHelp : List Exposed -> Parser E.Problem Exposing
exposingHelp revExposed =
    P.oneOf E.ExposingEnd
        [ P.symbol "," E.ExposingEnd
            |> P.andThen
                (\_ ->
                    Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentValue
                        |> P.andThen (\_ -> chompExposed)
                        |> P.andThen
                            (\exposed ->
                                Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                    |> P.andThen (\_ -> exposingHelp (exposed :: revExposed))
                            )
                )
        , P.symbol ")" E.ExposingEnd
            |> P.andThen (\_ -> P.succeed (Explicit (List.reverse revExposed)))
        ]


chompExposed : Parser E.Problem Exposed
chompExposed =
    P.getPosition
        |> P.andThen
            (\start ->
                P.oneOf E.ExposingValue
                    [ -- lower case name
                      Var.lower E.ExposingValue
                        |> P.andThen
                            (\name ->
                                P.getPosition
                                    |> P.andThen (\end -> P.succeed (Lower (Src.at start end name)))
                            )
                    , -- operator
                      P.symbol "(" E.ExposingValue
                        |> P.andThen
                            (\_ ->
                                Symbol.operator E.ExposingOperator E.ExposingOperatorReserved
                                    |> P.andThen
                                        (\op ->
                                            P.symbol ")" E.ExposingOperatorRightParen
                                                |> P.andThen
                                                    (\_ ->
                                                        P.getPosition
                                                            |> P.andThen
                                                                (\end ->
                                                                    P.succeed (Operator (Src.Region start end) op)
                                                                )
                                                    )
                                        )
                            )
                    , -- upper case name (type)
                      Var.upper E.ExposingValue
                        |> P.andThen
                            (\name ->
                                P.getPosition
                                    |> P.andThen
                                        (\end ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingIndentEnd
                                                |> P.andThen (\_ -> privacy)
                                                |> P.andThen (\priv -> P.succeed (Upper (Src.at start end name) priv))
                                        )
                            )
                    ]
            )


privacy : Parser E.Problem Privacy
privacy =
    P.oneOfWithFallback
        [ P.symbol "(" E.ExposingTypePrivacy
            |> P.andThen
                (\_ ->
                    Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
                        |> P.andThen (\_ -> P.getPosition)
                        |> P.andThen
                            (\start ->
                                P.symbol ".." E.ExposingTypePrivacy
                                    |> P.andThen (\_ -> P.getPosition)
                                    |> P.andThen
                                        (\end ->
                                            Space.chompAndCheckIndent E.ExposingSpace E.ExposingTypePrivacy
                                                |> P.andThen (\_ -> P.symbol ")" E.ExposingTypePrivacy)
                                                |> P.andThen (\_ -> P.succeed (Public (Src.Region start end)))
                                        )
                            )
                )
        ]
        Private



-- DECLARATIONS


chompDecls : List Decl.Decl -> Parser E.Problem (List Decl.Decl)
chompDecls decls =
    P.oneOfWithFallback
        [ Decl.declaration
            |> P.andThen
                (\( decl, _ ) ->
                    Space.chomp E.Declarations
                        |> P.andThen
                            (\_ ->
                                P.oneOfWithFallback
                                    [ freshLine E.DeclStart
                                        |> P.andThen (\_ -> chompDecls (decl :: decls))
                                    ]
                                    (List.reverse (decl :: decls))
                            )
                )
        ]
        (List.reverse decls)



-- HELPERS


freshLine : E.Problem -> Parser E.Problem ()
freshLine expecting =
    Space.chomp E.ModuleSpace
        |> P.andThen (\_ -> Space.checkFreshLine expecting)
