module Parse.Type exposing (expression, term, termNoApp)

import AST.Source as Src exposing (Position, Region, Type, Type_(..))
import Parse.Primitives as P exposing (Parser)
import Parse.Space as Space
import Parse.Variable as Var
import Reporting.Error.Syntax as E

expression : Parser E.Problem ( Type, Position )
expression =
  P.getPosition
    |>
    P.andThen
      (\start ->
        term
          |>
          P.andThen
                    (\( tipe, end ) -> Space.chomp E.TSpace |> P.andThen (\_ -> exprHelp start end tipe))
      )
exprHelp : Position -> Position -> Type -> Parser E.Problem ( Type, Position )
exprHelp start end tipe =
  P.oneOfWithFallback
    [ Space.checkIndent end E.TIndentStart
        |>
        P.andThen
          (\_ ->
            P.symbol "->" E.TStart
              |>
              P.andThen
                (\_ ->
                  Space.chompAndCheckIndent E.TSpace E.TIndentStart
                    |>
                    P.andThen
                      (\_ ->
                        expression
                          |>
                          P.andThen
                            (\( returnType, newEnd ) ->
                              let
                                funcType = Src.at start newEnd (TLambda tipe returnType)
                              in
                              P.succeed ( funcType, newEnd )
                            )
                      )
                )
          )
    ]
    ( tipe, end )
term : Parser E.Problem ( Type, Position )
term =
  P.getPosition
    |>
    P.andThen
      (\start ->
        P.oneOf
          E.TStart
          [ Var.lower E.TStart
              |>
              P.andThen
                        (\name -> P.getPosition |> P.andThen (\end -> P.succeed ( Src.at start end (TVar name), end )))
          , typeConstructor start
          , record start
          , tuple start
          ]
      )
typeConstructor : Position -> Parser E.Problem ( Type, Position )
typeConstructor start =
  Var.foreignUpper E.TStart
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
              Space.chomp E.TSpace |> P.andThen (\_ -> chompTypeArgs start region upper [])
            )
      )
chompTypeArgs :
  Position -> Region -> Var.Upper -> (List Type) -> Parser E.Problem ( Type, Position )
chompTypeArgs start region upper revArgs =
  P.getPosition
    |>
    P.andThen
      (\end ->
        P.oneOfWithFallback
          [ Space.checkIndent end E.TIndentStart
              |>
              P.andThen
                (\_ ->
                  termNoApp
                    |>
                    P.andThen
                      (\( arg, _ ) ->
                        Space.chomp E.TSpace |> P.andThen (\_ -> chompTypeArgs start region upper (arg :: revArgs))
                      )
                )
          ]
          ( Src.at
              start
              end
              (case upper of
                Var.Unqualified name    -> TType region name (List.reverse revArgs)
                Var.Qualified home name -> TTypeQual region home name (List.reverse revArgs)
              )
          , end
          )
      )
termNoApp : Parser E.Problem ( Type, Position )
termNoApp =
  P.getPosition
    |>
    P.andThen
      (\start ->
        P.oneOf
          E.TStart
          [ Var.lower E.TStart
              |>
              P.andThen
                        (\name -> P.getPosition |> P.andThen (\end -> P.succeed ( Src.at start end (TVar name), end )))
          , Var.foreignUpper E.TStart
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
                          ( Src.at
                              start
                              end
                              (case upper of
                                Var.Unqualified name    -> TType region name []
                                Var.Qualified home name -> TTypeQual region home name []
                              )
                          , end
                          )
                      )
                )
          , record start
          , tuple start
          ]
      )
record : Position -> Parser E.Problem ( Type, Position )
record start =
  P.inContext
    E.TRecord
    (P.symbol "{" E.TStart)
    (Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentOpen
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.TRecordOpen
            [ P.symbol "}" E.TRecordEnd
                |>
                P.andThen
                  (\_ ->
                    P.getPosition |> P.andThen (\end -> P.succeed ( Src.at start end (TRecord [] Nothing), end ))
                  )
            , Var.lower E.TRecordField
                |>
                P.andThen
                  (\name ->
                    P.getPosition
                      |>
                      P.andThen
                        (\nameEnd ->
                          Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                            |>
                            P.andThen
                              (\_ ->
                                let
                                  nameStart = { row = nameEnd.row, col = nameEnd.col - String.length name }

                                  locatedName = Src.at nameStart nameEnd name
                                in
                                P.oneOf
                                  E.TRecordColon
                                  [ P.symbol "|" E.TRecordColon
                                      |>
                                      P.andThen
                                        (\_ ->
                                          Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
                                            |>
                                            P.andThen (\_ -> recordField)
                                            |>
                                            P.andThen
                                              (\( field, fieldEnd ) ->
                                                Space.checkIndent fieldEnd E.TRecordIndentEnd
                                                  |>
                                                  P.andThen (\_ -> recordHelp start (Just locatedName) [ field ])
                                              )
                                        )
                                  , P.symbol ":" E.TRecordColon
                                      |>
                                      P.andThen
                                        (\_ ->
                                          Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                            |>
                                            P.andThen (\_ -> expression)
                                            |>
                                            P.andThen
                                              (\( fieldType, fieldEnd ) ->
                                                Space.checkIndent fieldEnd E.TRecordIndentEnd
                                                  |>
                                                  P.andThen (\_ -> recordHelp start Nothing [ ( locatedName, fieldType ) ])
                                              )
                                        )
                                  ]
                              )
                        )
                  )
            ]
        )
    )
recordHelp :
  Position
  -> (Maybe (Src.Located String))
     -> (List ( Src.Located String, Type )) -> Parser E.Problem ( Type, Position )
recordHelp start ext fields =
  P.oneOf
    E.TRecordEnd
    [ P.symbol "," E.TRecordEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentField
              |>
              P.andThen (\_ -> recordField)
              |>
              P.andThen
                (\( field, fieldEnd ) ->
                  Space.checkIndent fieldEnd E.TRecordIndentEnd
                    |>
                    P.andThen (\_ -> recordHelp start ext (field :: fields))
                )
          )
    , P.symbol "}" E.TRecordEnd
        |>
        P.andThen
          (\_ ->
            P.getPosition
              |>
              P.andThen (\end -> P.succeed ( Src.at start end (TRecord (List.reverse fields) ext), end ))
          )
    ]
recordField : Parser E.Problem ( ( Src.Located String, Type ), Position )
recordField =
  P.getPosition
    |>
    P.andThen
      (\fieldStart ->
        Var.lower E.TRecordField
          |>
          P.andThen
            (\name ->
              P.getPosition
                |>
                P.andThen
                  (\fieldEnd ->
                    Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentColon
                      |>
                      P.andThen
                        (\_ ->
                          P.symbol ":" E.TRecordColon
                            |>
                            P.andThen
                              (\_ ->
                                Space.chompAndCheckIndent E.TRecordSpace E.TRecordIndentType
                                  |>
                                  P.andThen (\_ -> expression)
                                  |>
                                  P.andThen
                                            (\( fieldType, end ) -> P.succeed ( ( Src.at fieldStart fieldEnd name, fieldType ), end ))
                              )
                        )
                  )
            )
      )
tuple : Position -> Parser E.Problem ( Type, Position )
tuple start =
  P.inContext
    E.TTuple
    (P.symbol "(" E.TStart)
    (Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentType1
      |>
      P.andThen
        (\_ ->
          P.oneOf
            E.TTupleOpen
            [ P.symbol ")" E.TTupleEnd
                |>
                P.andThen
                          (\_ -> P.getPosition |> P.andThen (\end -> P.succeed ( Src.at start end TUnit, end )))
            , expression
                |>
                P.andThen
                  (\( tipe, end ) ->
                    Space.checkIndent end E.TTupleIndentEnd |> P.andThen (\_ -> tupleHelp start tipe [])
                  )
            ]
        )
    )
tupleHelp : Position -> Type -> (List Type) -> Parser E.Problem ( Type, Position )
tupleHelp start firstType revTypes =
  P.oneOf
    E.TTupleEnd
    [ P.symbol "," E.TTupleEnd
        |>
        P.andThen
          (\_ ->
            Space.chompAndCheckIndent E.TTupleSpace E.TTupleIndentTypeN
              |>
              P.andThen (\_ -> expression)
              |>
              P.andThen
                (\( tipe, end ) ->
                  Space.checkIndent end E.TTupleIndentEnd
                    |>
                    P.andThen (\_ -> tupleHelp start firstType (tipe :: revTypes))
                )
          )
    , P.symbol ")" E.TTupleEnd
        |>
        P.andThen
          (\_ ->
            P.getPosition
              |>
              P.andThen
                (\end ->
                  case List.reverse revTypes of
                    []                       -> P.succeed ( firstType, end )
                    secondType :: otherTypes ->
                      P.succeed ( Src.at start end (TTuple firstType secondType otherTypes), end )
                )
          )
    ]
