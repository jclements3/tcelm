module Parse.Pattern exposing (term, expression)

import AST.Source as Src exposing (Pattern, Pattern_(..), Position, Region)
import Parse.Keyword as Keyword
import Parse.Number as Number
import Parse.Primitives as P exposing (Parser)
import Parse.Space as Space
import Parse.String as String
import Parse.Variable as Var
import Reporting.Error.Syntax as E

term : Parser E.Problem Pattern
term =
  P.getPosition
    |>
    P.andThen
              (\start -> P.oneOf E.PStart [ record start, tuple start, list start, termHelp start ])
termHelp : Position -> Parser E.Problem Pattern
termHelp start =
  P.oneOf
    E.PStart
    [ wildcard start
    , varLower start
    , varUpper start
    , numberLiteral start
    , stringLiteral start
    , charLiteral start ]
wildcard : Position -> Parser E.Problem Pattern
wildcard start =
  P.chompIf (\c -> c == '_') E.PStart
    |>
    P.andThen
      (\_ ->
        P.oneOfWithFallback
                            [ P.chompIf Char.isAlphaNum E.PStart |> P.andThen (\_ -> P.fail (E.PWildcardNotVar "_" 1)) ]
                            ()
      )
    |>
    P.andThen (\_ -> P.addEnd start PAnything)
varLower : Position -> Parser E.Problem Pattern
varLower start = Var.lower E.PStart |> P.andThen (\name -> P.addEnd start (PVar name))
varUpper : Position -> Parser E.Problem Pattern
varUpper start =
  Var.foreignUpper E.PStart
    |>
    P.andThen
      (\upper ->
        P.getPosition
          |>
          P.andThen
            (\end ->
              let
                region = Region start end
              in
              P.succeed
                (Src.at
                  start
                  end
                  (case upper of
                    Var.Unqualified name    -> PCtor region name []
                    Var.Qualified home name -> PCtorQual region home name []
                  )
                )
            )
      )
numberLiteral : Position -> Parser E.Problem Pattern
numberLiteral start =
  Number.number E.PStart E.PNumber
    |>
    P.andThen
      (\num ->
        case num of
          Number.Int n   -> P.addEnd start (PInt n)
          Number.Float _ -> P.fail (E.PFloat 0)
      )
stringLiteral : Position -> Parser E.Problem Pattern
stringLiteral start =
  String.string E.PStart E.PString |> P.andThen (\str -> P.addEnd start (PStr str))
charLiteral : Position -> Parser E.Problem Pattern
charLiteral start =
  String.character E.PStart E.PChar |> P.andThen (\chr -> P.addEnd start (PChr chr))
record : Position -> Parser E.Problem Pattern
record start =
  P.inContext
    E.PRecord
    (P.symbol "{" E.PStart)
    (Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentOpen
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.PRecordOpen
            [ P.addLocation (Var.lower E.PRecordField)
                |>
                P.andThen
                  (\var ->
                    Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                      |>
                      P.andThen (\_ -> recordHelp start [ var ])
                  )
            , P.symbol "}" E.PRecordEnd |> P.andThen (\_ -> P.addEnd start (PRecord []))
            ]
        )
    )
recordHelp : Position -> (List (Src.Located String)) -> Parser E.Problem Pattern
recordHelp start vars =
  P.oneOf
    E.PRecordEnd
    [ P.symbol "," E.PRecordEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentField
              |>
              P.andThen
                (\_ ->
                  P.addLocation (Var.lower E.PRecordField)
                    |>
                    P.andThen
                      (\var ->
                        Space.chompAndCheckIndent E.PRecordSpace E.PRecordIndentEnd
                          |>
                          P.andThen (\_ -> recordHelp start (var :: vars))
                      )
                )
          )
    , P.symbol "}" E.PRecordEnd |> P.andThen (\_ -> P.addEnd start (PRecord (List.reverse vars)))
    ]
tuple : Position -> Parser E.Problem Pattern
tuple start =
  P.inContext
    E.PTuple
    (P.symbol "(" E.PStart)
    (Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExpr1
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.PTupleOpen
            [ expression
                |>
                P.andThen
                  (\( pattern, end ) ->
                    Space.checkIndent end E.PTupleIndentEnd |> P.andThen (\_ -> tupleHelp start pattern [])
                  )
            , P.symbol ")" E.PTupleEnd |> P.andThen (\_ -> P.addEnd start PUnit)
            ]
        )
    )
tupleHelp : Position -> Pattern -> (List Pattern) -> Parser E.Problem Pattern
tupleHelp start firstPattern revPatterns =
  P.oneOf
    E.PTupleEnd
    [ P.symbol "," E.PTupleEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.PTupleSpace E.PTupleIndentExprN
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                      (\( pattern, end ) ->
                        Space.checkIndent end E.PTupleIndentEnd
                          |>
                          P.andThen (\_ -> tupleHelp start firstPattern (pattern :: revPatterns))
                      )
                )
          )
    , P.symbol ")" E.PTupleEnd
        |>
        P.andThen
          (\_ ->
            case List.reverse revPatterns of
              []                             -> P.succeed firstPattern
              secondPattern :: otherPatterns ->
                P.addEnd start (PTuple firstPattern secondPattern otherPatterns)
          )
    ]
list : Position -> Parser E.Problem Pattern
list start =
  P.inContext
    E.PList
    (P.symbol "[" E.PStart)
    (Space.chompAndCheckIndent E.PListSpace E.PListIndentOpen
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.PListOpen
            [ expression
                |>
                P.andThen
                  (\( pattern, end ) ->
                    Space.checkIndent end E.PListIndentEnd |> P.andThen (\_ -> listHelp start [ pattern ])
                  )
            , P.symbol "]" E.PListEnd |> P.andThen (\_ -> P.addEnd start (PList []))
            ]
        )
    )
listHelp : Position -> (List Pattern) -> Parser E.Problem Pattern
listHelp start patterns =
  P.oneOf
    E.PListEnd
    [ P.symbol "," E.PListEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.PListSpace E.PListIndentExpr
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                      (\( pattern, end ) ->
                        Space.checkIndent end E.PListIndentEnd
                          |>
                          P.andThen (\_ -> listHelp start (pattern :: patterns))
                      )
                )
          )
    , P.symbol "]" E.PListEnd |> P.andThen (\_ -> P.addEnd start (PList (List.reverse patterns)))
    ]
expression : Parser E.Problem ( Pattern, Position )
expression =
  P.getPosition
    |>
    P.andThen (\start -> exprPart |> P.andThen (\ePart -> exprHelp start [] ePart))
exprHelp :
  Position -> (List Pattern) -> ( Pattern, Position ) -> Parser E.Problem ( Pattern, Position )
exprHelp start revPatterns ( pattern, end ) =
  P.oneOfWithFallback
    [ Space.checkIndent end E.PIndentStart
        |>
        P.andThen
          (\_ ->
            P.symbol "::" E.PStart
              |>
              P.andThen
                (\_ ->
                  Space.chompAndCheckIndent E.PSpace E.PIndentStart
                    |>
                    P.andThen
                              (\_ -> exprPart |> P.andThen (\ePart -> exprHelp start (pattern :: revPatterns) ePart))
                )
          )
    , Space.checkIndent end E.PIndentStart
        |>
        P.andThen
          (\_ ->
            Keyword.as_ E.PStart
              |>
              P.andThen
                (\_ ->
                  Space.chompAndCheckIndent E.PSpace E.PIndentAlias
                    |>
                    P.andThen
                      (\_ ->
                        P.getPosition
                          |>
                          P.andThen
                            (\nameStart ->
                              Var.lower E.PAlias
                                |>
                                P.andThen
                                  (\name ->
                                    P.getPosition
                                      |>
                                      P.andThen
                                        (\newEnd ->
                                          Space.chomp E.PSpace
                                            |>
                                            P.andThen
                                              (\_ ->
                                                let
                                                  alias = Src.at nameStart newEnd name

                                                  finalPattern = Src.at start newEnd (PAlias (cons revPatterns pattern) alias)
                                                in
                                                P.succeed ( finalPattern, newEnd )
                                              )
                                        )
                                  )
                            )
                      )
                )
          )
    ]
    ( cons revPatterns pattern, end )
cons : (List Pattern) -> Pattern -> Pattern
cons revPatterns lastPat =
  case revPatterns of
    []              -> lastPat
    prevPat :: rest -> cons rest (Src.merge prevPat lastPat (PCons prevPat lastPat))
exprPart : Parser E.Problem ( Pattern, Position )
exprPart =
  P.oneOf
    E.PStart
    [ P.getPosition
        |>
        P.andThen
          (\start ->
            Var.foreignUpper E.PStart
              |>
              P.andThen
                        (\upper -> P.getPosition |> P.andThen (\end -> exprTermHelp (Region start end) upper start []))
          )
    , term
        |>
        P.andThen
          (\eterm ->
            let
              (Src.At region _) = eterm

              end = region.end
            in
            Space.chomp E.PSpace |> P.andThen (\_ -> P.succeed ( eterm, end ))
          )
    ]
exprTermHelp :
  Region -> Var.Upper -> Position -> (List Pattern) -> Parser E.Problem ( Pattern, Position )
exprTermHelp region upper start revArgs =
  P.getPosition
    |>
    P.andThen
      (\end ->
        Space.chomp E.PSpace
          |>
          P.andThen
            (\_ ->
              P.oneOfWithFallback
                [ Space.checkIndent end E.PIndentStart
                    |>
                    P.andThen (\_ -> term |> P.andThen (\arg -> exprTermHelp region upper start (arg :: revArgs)))
                ]
                ( Src.at
                    start
                    end
                    (case upper of
                      Var.Unqualified name    -> PCtor region name (List.reverse revArgs)
                      Var.Qualified home name -> PCtorQual region home name (List.reverse revArgs)
                    )
                , end
                )
            )
      )
