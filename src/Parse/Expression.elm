module Parse.Expression exposing (expression)

import AST.Source as Src exposing (Def(..), Expr, Expr_(..), Position, Region, VarType(..))
import Parse.Keyword as Keyword
import Parse.Number as Number
import Parse.Pattern as Pattern
import Parse.Primitives as P exposing (Parser)
import Parse.Space as Space
import Parse.String as String
import Parse.Symbol as Symbol
import Parse.Type as Type
import Parse.Variable as Var
import Reporting.Error.Syntax as E

type alias State =
  { ops : List ( Expr, Src.Located String ), expr : Expr, args : List Expr, end : Position }
type alias Field = ( Src.Located String, Expr )

expression : Parser E.Problem ( Expr, Position )
expression =
  P.getPosition
    |>
    P.andThen
      (\start ->
        P.oneOf
          E.Start
          [ let_ start
          , if_ start
          , case_ start
          , do_ start
          , lambda start
          , possiblyNegativeTerm start
              |>
              P.andThen
                (\expr ->
                  P.getPosition
                    |>
                    P.andThen
                              (\end -> Space.chomp E.Space |> P.andThen (\_ -> chompExprEnd start (State [] expr [] end)))
                )
          ]
      )
chompExprEnd : Position -> State -> Parser E.Problem ( Expr, Position )
chompExprEnd start { ops, expr, args, end } =
  P.oneOfWithFallback
    [ P.backtrackable
        (Space.checkIndent end E.Start
          |>
          P.andThen
            (\_ ->
              term
                |>
                P.andThen
                  (\arg ->
                    P.getPosition
                      |>
                      P.andThen
                        (\newEnd ->
                          Space.chomp E.Space
                            |>
                            P.andThen (\_ -> chompExprEnd start (State ops expr (arg :: args) newEnd))
                        )
                  )
            )
        )
    , P.backtrackable (Space.checkIndent end E.Start)
        |>
        P.andThen
          (\_ ->
            P.addLocation (Symbol.operator E.Start E.OperatorReserved)
              |>
              P.andThen
                (\op ->
                  let
                    (Src.At region opName) = op

                    opStart = region.start

                    opEnd = region.end
                  in
                  Space.chompAndCheckIndent E.Space (E.IndentOperatorRight opName)
                    |>
                    P.andThen
                      (\_ ->
                        P.getPosition
                          |>
                          P.andThen
                            (\newStart ->
                              if opName == "-" && end /= opStart && opEnd == newStart then
                                term
                                  |>
                                  P.andThen
                                    (\negatedExpr ->
                                      P.getPosition
                                        |>
                                        P.andThen
                                          (\newEnd ->
                                            Space.chomp E.Space
                                              |>
                                              P.andThen
                                                (\_ ->
                                                  let
                                                    arg = Src.at opStart newEnd (Negate negatedExpr)
                                                  in
                                                  chompExprEnd start (State ops expr (arg :: args) newEnd)
                                                )
                                          )
                                    )

                              else
                                P.oneOf
                                  (E.OperatorRight opName)
                                  [ possiblyNegativeTerm newStart
                                      |>
                                      P.andThen
                                        (\newExpr ->
                                          P.getPosition
                                            |>
                                            P.andThen
                                              (\newEnd ->
                                                Space.chomp E.Space
                                                  |>
                                                  P.andThen
                                                    (\_ ->
                                                      let
                                                        newOps = ( toCall expr args, op ) :: ops
                                                      in
                                                      chompExprEnd start (State newOps newExpr [] newEnd)
                                                    )
                                              )
                                        )
                                  , P.oneOf (E.OperatorRight opName)
                                            [ let_ newStart, case_ newStart, if_ newStart, lambda newStart ]
                                      |>
                                      P.andThen
                                        (\( newLast, newEnd ) ->
                                          let
                                            newOps = ( toCall expr args, op ) :: ops

                                            finalExpr = Src.at start newEnd (Binops (List.reverse newOps) newLast)
                                          in
                                          P.succeed ( finalExpr, newEnd )
                                        )
                                  ]
                            )
                      )
                )
          )
    ]
    (case ops of
      [] -> ( toCall expr args, end )
      _  -> ( Src.at start end (Binops (List.reverse ops) (toCall expr args)), end )
    )
possiblyNegativeTerm : Position -> Parser E.Problem Expr
possiblyNegativeTerm start =
  P.oneOf
    E.Start
    [ P.symbol "-" E.Start
        |>
        P.andThen (\_ -> term |> P.andThen (\expr -> P.addEnd start (Negate expr)))
    , term
    ]
toCall : Expr -> (List Expr) -> Expr
toCall func revArgs =
  case revArgs of
    []           -> func
    lastArg :: _ -> Src.merge func lastArg (Call func (List.reverse revArgs))
term : Parser E.Problem Expr
term =
  P.getPosition
    |>
    P.andThen
      (\start ->
        P.oneOf
          E.Start
          [ variable start |> P.andThen (accessible start)
          , stringLiteral start
          , numberLiteral start
          , list start
          , record start |> P.andThen (accessible start)
          , tuple start |> P.andThen (accessible start)
          , accessor start
          , charLiteral start ]
      )
stringLiteral : Position -> Parser E.Problem Expr
stringLiteral start =
  String.string E.Start E.String |> P.andThen (\str -> P.addEnd start (Str str))
charLiteral : Position -> Parser E.Problem Expr
charLiteral start =
  String.character E.Start E.Char |> P.andThen (\chr -> P.addEnd start (Chr chr))
numberLiteral : Position -> Parser E.Problem Expr
numberLiteral start =
  Number.number E.Start E.Number
    |>
    P.andThen
      (\num ->
        P.addEnd
          start
          (case num of
            Number.Int n   -> Int n
            Number.Float f -> Float f
          )
      )
accessor : Position -> Parser E.Problem Expr
accessor start =
  P.symbol "." E.Dot
    |>
    P.andThen (\_ -> Var.lower E.Access |> P.andThen (\field -> P.addEnd start (Accessor field)))
variable : Position -> Parser E.Problem Expr
variable start = Var.foreignAlpha E.Start |> P.andThen (\var -> P.addEnd start var)
accessible : Position -> Expr -> Parser E.Problem Expr
accessible start expr =
  P.oneOfWithFallback
    [ P.symbol "." E.Dot
        |>
        P.andThen
          (\_ ->
            P.getPosition
              |>
              P.andThen
                (\pos ->
                  Var.lower E.Access
                    |>
                    P.andThen
                      (\field ->
                        P.getPosition
                          |>
                          P.andThen (\end -> accessible start (Src.at start end (Access expr (Src.at pos end field))))
                      )
                )
          )
    ]
    expr
list : Position -> Parser E.Problem Expr
list start =
  P.inContext
    E.List
    (P.symbol "[" E.Start)
    (Space.chompAndCheckIndent E.ListSpace E.ListIndentOpen
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.ListOpen
            [ expression
                |>
                P.andThen
                  (\( entry, end ) ->
                    Space.checkIndent end E.ListIndentEnd |> P.andThen (\_ -> chompListEnd start [ entry ])
                  )
            , P.symbol "]" E.ListOpen |> P.andThen (\_ -> P.addEnd start (List []))
            ]
        )
    )
chompListEnd : Position -> (List Expr) -> Parser E.Problem Expr
chompListEnd start entries =
  P.oneOf
    E.ListEnd
    [ P.symbol "," E.ListEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.ListSpace E.ListIndentExpr
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                      (\( entry, end ) ->
                        Space.checkIndent end E.ListIndentEnd
                          |>
                          P.andThen (\_ -> chompListEnd start (entry :: entries))
                      )
                )
          )
    , P.symbol "]" E.ListEnd |> P.andThen (\_ -> P.addEnd start (List (List.reverse entries)))
    ]
tuple : Position -> Parser E.Problem Expr
tuple start =
  P.inContext
    E.Tuple
    (P.symbol "(" E.Start)
    (Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExpr1
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.TupleIndentExpr1
            [ Symbol.operator E.TupleIndentExpr1 E.TupleOperatorReserved
                |>
                P.andThen
                  (\op ->
                    if op == "-" then
                      P.oneOf
                        E.TupleOperatorClose
                        [ P.symbol ")" E.TupleOperatorClose |> P.andThen (\_ -> P.addEnd start (Op op))
                        , term
                            |>
                            P.andThen
                              (\negatedExpr ->
                                P.getPosition
                                  |>
                                  P.andThen
                                    (\negEnd ->
                                      Space.chomp E.Space
                                        |>
                                        P.andThen
                                          (\_ ->
                                            let
                                              exprStart = { row = start.row, col = start.col + 1 }

                                              expr = Src.at exprStart negEnd (Negate negatedExpr)
                                            in
                                            chompExprEnd exprStart (State [] expr [] negEnd)
                                              |>
                                              P.andThen
                                                (\( entry, end ) ->
                                                  Space.checkIndent end E.TupleIndentEnd |> P.andThen (\_ -> chompTupleEnd start entry [])
                                                )
                                          )
                                    )
                              )
                        ]

                    else
                      P.symbol ")" E.TupleOperatorClose |> P.andThen (\_ -> P.addEnd start (Op op))
                  )
            , P.symbol ")" E.TupleIndentExpr1 |> P.andThen (\_ -> P.addEnd start Unit)
            , expression
                |>
                P.andThen
                  (\( entry, end ) ->
                    Space.checkIndent end E.TupleIndentEnd |> P.andThen (\_ -> chompTupleEnd start entry [])
                  )
            ]
        )
    )
chompTupleEnd : Position -> Expr -> (List Expr) -> Parser E.Problem Expr
chompTupleEnd start firstExpr revExprs =
  P.oneOf
    E.TupleEnd
    [ P.symbol "," E.TupleEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.TupleSpace E.TupleIndentExprN
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                      (\( entry, end ) ->
                        Space.checkIndent end E.TupleIndentEnd
                          |>
                          P.andThen (\_ -> chompTupleEnd start firstExpr (entry :: revExprs))
                      )
                )
          )
    , P.symbol ")" E.TupleEnd
        |>
        P.andThen
          (\_ ->
            case List.reverse revExprs of
              []                       -> P.succeed firstExpr
              secondExpr :: otherExprs -> P.addEnd start (Tuple firstExpr secondExpr otherExprs)
          )
    ]
record : Position -> Parser E.Problem Expr
record start =
  P.inContext
    E.Record
    (P.symbol "{" E.Start)
    (Space.chompAndCheckIndent E.RecordSpace E.RecordIndentOpen
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.RecordOpen
            [ P.symbol "}" E.RecordOpen |> P.andThen (\_ -> P.addEnd start (Record []))
            , P.addLocation (Var.lower E.RecordField)
                |>
                P.andThen
                  (\starter ->
                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
                      |>
                      P.andThen
                        (\_ ->
                          P.oneOf
                            E.RecordEquals
                            [ P.symbol "|" E.RecordEquals
                                |>
                                P.andThen
                                  (\_ ->
                                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
                                      |>
                                      P.andThen (\_ -> chompField)
                                      |>
                                      P.andThen
                                        (\firstField ->
                                          chompFields [ firstField ] |> P.andThen (\fields -> P.addEnd start (Update starter fields))
                                        )
                                  )
                            , P.symbol "=" E.RecordEquals
                                |>
                                P.andThen
                                  (\_ ->
                                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
                                      |>
                                      P.andThen
                                        (\_ ->
                                          expression
                                            |>
                                            P.andThen
                                              (\( value, end ) ->
                                                Space.checkIndent end E.RecordIndentEnd
                                                  |>
                                                  P.andThen
                                                    (\_ ->
                                                      chompFields [ ( starter, value ) ] |> P.andThen (\fields -> P.addEnd start (Record fields))
                                                    )
                                              )
                                        )
                                  )
                            ]
                        )
                  )
            ]
        )
    )
chompFields : (List Field) -> Parser E.Problem (List Field)
chompFields fields =
  P.oneOf
    E.RecordEnd
    [ P.symbol "," E.RecordEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.RecordSpace E.RecordIndentField
              |>
              P.andThen (\_ -> chompField)
              |>
              P.andThen (\f -> chompFields (f :: fields))
          )
    , P.symbol "}" E.RecordEnd |> P.andThen (\_ -> P.succeed (List.reverse fields))
    ]
chompField : Parser E.Problem Field
chompField =
  P.addLocation (Var.lower E.RecordField)
    |>
    P.andThen
      (\key ->
        Space.chompAndCheckIndent E.RecordSpace E.RecordIndentEquals
          |>
          P.andThen
            (\_ ->
              P.symbol "=" E.RecordEquals
                |>
                P.andThen
                  (\_ ->
                    Space.chompAndCheckIndent E.RecordSpace E.RecordIndentExpr
                      |>
                      P.andThen
                        (\_ ->
                          expression
                            |>
                            P.andThen
                              (\( value, end ) ->
                                Space.checkIndent end E.RecordIndentEnd |> P.andThen (\_ -> P.succeed ( key, value ))
                              )
                        )
                  )
            )
      )
if_ : Position -> Parser E.Problem ( Expr, Position )
if_ start = P.inContext E.If (Keyword.if_ E.Start) (chompIfEnd start [])
chompIfEnd : Position -> (List ( Expr, Expr )) -> Parser E.Problem ( Expr, Position )
chompIfEnd start branches =
  Space.chompAndCheckIndent E.IfSpace E.IfIndentCondition
    |>
    P.andThen
      (\_ ->
        expression
          |>
          P.andThen
            (\( condition, condEnd ) ->
              Space.checkIndent condEnd E.IfIndentThen
                |>
                P.andThen
                  (\_ ->
                    Keyword.then_ E.IfThen
                      |>
                      P.andThen
                        (\_ ->
                          Space.chompAndCheckIndent E.IfSpace E.IfIndentThenBranch
                            |>
                            P.andThen
                              (\_ ->
                                expression
                                  |>
                                  P.andThen
                                    (\( thenBranch, thenEnd ) ->
                                      Space.checkIndent thenEnd E.IfIndentElse
                                        |>
                                        P.andThen
                                          (\_ ->
                                            Keyword.else_ E.IfElse
                                              |>
                                              P.andThen
                                                (\_ ->
                                                  Space.chompAndCheckIndent E.IfSpace E.IfIndentElseBranch
                                                    |>
                                                    P.andThen
                                                      (\_ ->
                                                        let
                                                          newBranches = ( condition, thenBranch ) :: branches
                                                        in
                                                        P.oneOf
                                                          E.IfElseBranchStart
                                                          [ Keyword.if_ E.IfElseBranchStart |> P.andThen (\_ -> chompIfEnd start newBranches)
                                                          , expression
                                                              |>
                                                              P.andThen
                                                                (\( elseBranch, elseEnd ) ->
                                                                  let
                                                                    ifExpr = Src.at start elseEnd (If (List.reverse newBranches) elseBranch)
                                                                  in
                                                                  P.succeed ( ifExpr, elseEnd )
                                                                )
                                                          ]
                                                      )
                                                )
                                          )
                                    )
                              )
                        )
                  )
            )
      )
lambda : Position -> Parser E.Problem ( Expr, Position )
lambda start =
  P.inContext
    E.Func
    (P.symbol "\\" E.Start)
    (Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArg
      |>
      P.andThen
        (\_ ->
          Pattern.term
            |>
            P.andThen
              (\arg ->
                Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
                  |>
                  P.andThen (\_ -> chompArgs [ arg ])
                  |>
                  P.andThen
                    (\revArgs ->
                      Space.chompAndCheckIndent E.FuncSpace E.FuncIndentBody
                        |>
                        P.andThen
                          (\_ ->
                            expression
                              |>
                              P.andThen
                                (\( body, end ) ->
                                  let
                                    funcExpr = Src.at start end (Lambda (List.reverse revArgs) body)
                                  in
                                  P.succeed ( funcExpr, end )
                                )
                          )
                    )
              )
        )
    )
chompArgs : (List Src.Pattern) -> Parser E.Problem (List Src.Pattern)
chompArgs revArgs =
  P.oneOf
    E.FuncArrow
    [ Pattern.term
        |>
        P.andThen
          (\arg ->
            Space.chompAndCheckIndent E.FuncSpace E.FuncIndentArrow
              |>
              P.andThen (\_ -> chompArgs (arg :: revArgs))
          )
    , P.symbol "->" E.FuncArrow |> P.andThen (\_ -> P.succeed revArgs)
    ]
case_ : Position -> Parser E.Problem ( Expr, Position )
case_ start =
  P.inContext
    E.Case
    (Keyword.case_ E.Start)
    (Space.chompAndCheckIndent E.CaseSpace E.CaseIndentExpr
      |>
      P.andThen
        (\_ ->
          expression
            |>
            P.andThen
              (\( expr, exprEnd ) ->
                Space.checkIndent exprEnd E.CaseIndentOf
                  |>
                  P.andThen
                    (\_ ->
                      Keyword.of_ E.CaseOf
                        |>
                        P.andThen
                          (\_ ->
                            Space.chompAndCheckIndent E.CaseSpace E.CaseIndentPattern
                              |>
                              P.andThen
                                (\_ ->
                                  P.withIndent
                                    (chompBranch
                                      |>
                                      P.andThen
                                        (\( firstBranch, firstEnd ) ->
                                          chompCaseEnd [ firstBranch ] firstEnd
                                            |>
                                            P.andThen (\( branches, end ) -> P.succeed ( Src.at start end (Case expr branches), end ))
                                        )
                                    )
                                )
                          )
                    )
              )
        )
    )
chompBranch : Parser E.Problem ( ( Src.Pattern, Maybe Expr, Expr ), Position )
chompBranch =
  Pattern.expression
    |>
    P.andThen
      (\( pattern, patternEnd ) ->
        Space.checkIndent patternEnd E.CaseIndentArrow
          |>
          P.andThen
            (\_ ->
              chompOptionalGuard
                |>
                P.andThen
                  (\maybeGuard ->
                    P.symbol "->" E.CaseArrow
                      |>
                      P.andThen
                        (\_ ->
                          Space.chompAndCheckIndent E.CaseSpace E.CaseIndentBranch
                            |>
                            P.andThen
                              (\_ ->
                                expression
                                  |>
                                  P.andThen (\( branchExpr, end ) -> P.succeed ( ( pattern, maybeGuard, branchExpr ), end ))
                              )
                        )
                  )
            )
      )
{-| Parse optional guard expression: "if <expr>"
        Returns Nothing if no guard, Just expr if guard present. -}
chompOptionalGuard : Parser E.Problem (Maybe Expr)
chompOptionalGuard =
  P.oneOfWithFallback
    [ Keyword.if_ E.Start
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.CaseSpace E.CaseIndentArrow
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                              (\( guardExpr, _ ) -> Space.chomp E.Space |> P.andThen (\_ -> P.succeed (Just guardExpr)))
                )
          )
    ]
    Nothing
chompCaseEnd :
  (List ( Src.Pattern, Maybe Expr, Expr ))
  -> Position -> Parser E.Problem ( List ( Src.Pattern, Maybe Expr, Expr ), Position )
chompCaseEnd branches end =
  P.oneOfWithFallback
    [ Space.checkAligned E.CasePatternAlignment
        |>
        P.andThen
          (\_ ->
            chompBranch |> P.andThen (\( branch, newEnd ) -> chompCaseEnd (branch :: branches) newEnd)
          )
    ]
    ( List.reverse branches, end )
do_ : Position -> Parser E.Problem ( Expr, Position )
do_ start =
  P.inContext
    E.Let
    (Keyword.do_ E.Start)
    (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
      |>
      P.andThen
        (\_ ->
          P.withIndent (chompDoStatements [])
            |>
            P.andThen (\( stmts, end ) -> P.succeed ( Src.at start end (Do stmts), end ))
        )
    )
chompDoStatements :
  (List Src.DoStatement) -> Parser E.Problem ( List Src.DoStatement, Position )
chompDoStatements revStmts =
  P.getPosition
    |>
    P.andThen
      (\pos ->
        P.oneOf
          E.Start
          [ Keyword.let_ E.Start
              |>
              P.andThen
                (\_ ->
                  Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                    |>
                    P.andThen
                      (\_ ->
                        P.withIndent (chompLetDef |> P.andThen (\( def, defEnd ) -> chompLetDefs [ def ] defEnd))
                          |>
                          P.andThen (\( defs, defsEnd ) -> chompMoreDoStatements (Src.DoLet defs :: revStmts) defsEnd)
                      )
                )
          , P.backtrackable
              (Pattern.term
                |>
                P.andThen
                  (\pattern ->
                    Space.chomp E.Space
                      |>
                      P.andThen
                        (\_ ->
                          Symbol.leftArrow E.Start
                            |>
                            P.andThen
                              (\_ ->
                                Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
                                  |>
                                  P.andThen
                                    (\_ ->
                                      expression |> P.andThen (\( expr, exprEnd ) -> P.succeed ( Src.DoBind pattern expr, exprEnd ))
                                    )
                              )
                        )
                  )
              )
              |>
              P.andThen (\( stmt, stmtEnd ) -> chompMoreDoStatements (stmt :: revStmts) stmtEnd)
          , expression
              |>
              P.andThen (\( expr, end ) -> chompMoreDoStatements (Src.DoExpr expr :: revStmts) end)
          ]
      )
chompMoreDoStatements :
  (List Src.DoStatement) -> Position -> Parser E.Problem ( List Src.DoStatement, Position )
chompMoreDoStatements revStmts end =
  P.oneOfWithFallback
                      [ Space.checkAligned E.LetDefAlignment |> P.andThen (\_ -> chompDoStatements revStmts) ]
                      ( List.reverse revStmts, end )
let_ : Position -> Parser E.Problem ( Expr, Position )
let_ start =
  P.inContext
    E.Let
    (Keyword.let_ E.Start)
    (Space.chompAndCheckIndent E.LetSpace E.LetIndentDef
      |>
      P.andThen
        (\_ ->
          P.withIndent (chompLetDef |> P.andThen (\( def, defEnd ) -> chompLetDefs [ def ] defEnd))
            |>
            P.andThen
              (\( defs, defsEnd ) ->
                Space.checkIndent defsEnd E.LetIndentIn
                  |>
                  P.andThen
                    (\_ ->
                      Keyword.in_ E.LetIn
                        |>
                        P.andThen
                          (\_ ->
                            Space.chompAndCheckIndent E.LetSpace E.LetIndentBody
                              |>
                              P.andThen
                                (\_ ->
                                  expression
                                    |>
                                    P.andThen (\( body, end ) -> P.succeed ( Src.at start end (Let defs body), end ))
                                )
                          )
                    )
              )
        )
    )
chompLetDefs :
  (List (Src.Located Def)) -> Position -> Parser E.Problem ( List (Src.Located Def), Position )
chompLetDefs revDefs end =
  P.oneOfWithFallback
    [ Space.checkAligned E.LetDefAlignment
        |>
        P.andThen
                  (\_ -> chompLetDef |> P.andThen (\( def, defEnd ) -> chompLetDefs (def :: revDefs) defEnd))
    ]
    ( List.reverse revDefs, end )
chompLetDef : Parser E.Problem ( Src.Located Def, Position )
chompLetDef = P.oneOf E.LetDefName [ definition, destructure ]
definition : Parser E.Problem ( Src.Located Def, Position )
definition =
  P.getPosition
    |>
    P.andThen
      (\start ->
        P.addLocation (Var.lower E.LetDefName)
          |>
          P.andThen
            (\((Src.At _ name) as aname) ->
              Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                |>
                P.andThen
                  (\_ ->
                    P.oneOf
                      E.DefEquals
                      [ P.symbol ":" E.DefEquals
                          |>
                          P.andThen
                            (\_ ->
                              Space.chompAndCheckIndent E.DefSpace E.DefIndentType
                                |>
                                P.andThen (\_ -> Type.expression)
                                |>
                                P.andThen
                                  (\( tipe, _ ) ->
                                    Space.checkAligned E.DefAlignment
                                      |>
                                      P.andThen (\_ -> chompMatchingName name)
                                      |>
                                      P.andThen
                                        (\defName ->
                                          Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                                            |>
                                            P.andThen (\_ -> chompDefArgsAndBody start defName (Just tipe) [])
                                        )
                                  )
                            )
                      , chompDefArgsAndBody start aname Nothing []
                      ]
                  )
            )
      )
chompDefArgsAndBody :
  Position
  -> (Src.Located String)
     -> (Maybe Src.Type) -> (List Src.Pattern) -> Parser E.Problem ( Src.Located Def, Position )
chompDefArgsAndBody start name tipe revArgs =
  P.oneOf
    E.DefEquals
    [ Pattern.term
        |>
        P.andThen
          (\arg ->
            Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
              |>
              P.andThen (\_ -> chompDefArgsAndBody start name tipe (arg :: revArgs))
          )
    , P.symbol "=" E.DefEquals
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.DefSpace E.DefIndentBody
              |>
              P.andThen
                (\_ ->
                  expression
                    |>
                    P.andThen
                      (\( body, end ) ->
                        P.succeed ( Src.at start end (Define name (List.reverse revArgs) body tipe), end )
                      )
                )
          )
    ]
chompMatchingName : String -> Parser E.Problem (Src.Located String)
chompMatchingName expectedName =
  P.addLocation (Var.lower E.DefNameRepeat)
    |>
    P.andThen
      (\locatedName ->
        let
          (Src.At _ name) = locatedName
        in
        if name == expectedName then
          P.succeed locatedName

        else
          P.fail (E.LetDef expectedName)
      )
destructure : Parser E.Problem ( Src.Located Def, Position )
destructure =
  P.getPosition
    |>
    P.andThen
      (\start ->
        Pattern.term
          |>
          P.andThen
            (\pattern ->
              Space.chompAndCheckIndent E.DefSpace E.DefIndentEquals
                |>
                P.andThen
                  (\_ ->
                    P.symbol "=" E.DefEquals
                      |>
                      P.andThen
                        (\_ ->
                          Space.chompAndCheckIndent E.DefSpace E.DefIndentBody
                            |>
                            P.andThen
                              (\_ ->
                                expression
                                  |>
                                  P.andThen (\( expr, end ) -> P.succeed ( Src.at start end (Destruct pattern expr), end ))
                              )
                        )
                  )
            )
      )
