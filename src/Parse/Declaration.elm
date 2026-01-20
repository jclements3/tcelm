module Parse.Declaration exposing (Decl(..), declaration)

{-| Declaration parsing for Elm.
-}

import AST.Source as Src exposing (Alias, Infix, Port, Position, Union, Value)
import Parse.Expression as Expr
import Parse.Keyword as Keyword
import Parse.Number as Number
import Parse.Pattern as Pattern
import Parse.Primitives as P exposing (Parser)
import Parse.Space as Space
import Parse.Symbol as Symbol
import Parse.Type as Type
import Parse.Variable as Var
import Reporting.Error.Syntax as E


type Decl
    = Value (Src.Located Value)
    | Union (Src.Located Union)
    | Alias (Src.Located Alias)
    | PortDecl Port


declaration : Parser E.Problem ( Decl, Position )
declaration =
    P.getPosition
        |> P.andThen
            (\start ->
                P.oneOf E.DeclStart
                    [ typeDecl start
                    , portDecl start
                    , valueDecl start
                    ]
            )



-- TYPE DECLARATIONS


typeDecl : Position -> Parser E.Problem ( Decl, Position )
typeDecl start =
    Keyword.type_ E.DeclStart
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                    |> P.andThen
                        (\_ ->
                            P.oneOf E.DeclStart
                                [ -- type alias
                                  Keyword.alias_ E.DeclStart
                                    |> P.andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                                |> P.andThen (\_ -> typeAlias start)
                                        )
                                , -- type union
                                  typeUnion start
                                ]
                        )
            )


typeAlias : Position -> Parser E.Problem ( Decl, Position )
typeAlias start =
    P.addLocation (Var.upper E.DeclStart)
        |> P.andThen
            (\name ->
                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                    |> P.andThen (\_ -> chompTypeArgs [])
                    |> P.andThen
                        (\args ->
                            P.symbol "=" E.DeclStart
                                |> P.andThen
                                    (\_ ->
                                        Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                            |> P.andThen (\_ -> Type.expression)
                                            |> P.andThen
                                                (\( tipe, end ) ->
                                                    let
                                                        alias =
                                                            Src.Alias name (List.reverse args) tipe
                                                    in
                                                    P.succeed
                                                        ( Alias (Src.at start end alias)
                                                        , end
                                                        )
                                                )
                                    )
                        )
            )


typeUnion : Position -> Parser E.Problem ( Decl, Position )
typeUnion start =
    P.addLocation (Var.upper E.DeclStart)
        |> P.andThen
            (\name ->
                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                    |> P.andThen (\_ -> chompTypeArgs [])
                    |> P.andThen
                        (\args ->
                            P.symbol "=" E.DeclStart
                                |> P.andThen
                                    (\_ ->
                                        Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                            |> P.andThen (\_ -> chompVariants [])
                                            |> P.andThen
                                                (\( variants, end ) ->
                                                    let
                                                        union =
                                                            Src.Union name (List.reverse args) variants
                                                    in
                                                    P.succeed
                                                        ( Union (Src.at start end union)
                                                        , end
                                                        )
                                                )
                                    )
                        )
            )


chompTypeArgs : List (Src.Located String) -> Parser E.Problem (List (Src.Located String))
chompTypeArgs revArgs =
    P.oneOfWithFallback
        [ P.addLocation (Var.lower E.DeclStart)
            |> P.andThen
                (\arg ->
                    Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                        |> P.andThen (\_ -> chompTypeArgs (arg :: revArgs))
                )
        ]
        (List.reverse revArgs)


chompVariants : List ( Src.Located String, List Src.Type ) -> Parser E.Problem ( List ( Src.Located String, List Src.Type ), Position )
chompVariants revVariants =
    chompVariant
        |> P.andThen
            (\( variant, end ) ->
                let
                    newVariants =
                        variant :: revVariants
                in
                Space.chomp E.DeclSpace
                    |> P.andThen
                        (\_ ->
                            P.oneOfWithFallback
                                [ P.symbol "|" E.DeclStart
                                    |> P.andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                                |> P.andThen (\_ -> chompVariants newVariants)
                                        )
                                ]
                                ( List.reverse newVariants, end )
                        )
            )


chompVariant : Parser E.Problem ( ( Src.Located String, List Src.Type ), Position )
chompVariant =
    P.addLocation (Var.upper E.DeclStart)
        |> P.andThen
            (\name ->
                Space.chomp E.DeclSpace
                    |> P.andThen (\_ -> chompVariantArgs [])
                    |> P.andThen
                        (\( args, end ) ->
                            P.succeed ( ( name, List.reverse args ), end )
                        )
            )


chompVariantArgs : List Src.Type -> Parser E.Problem ( List Src.Type, Position )
chompVariantArgs revArgs =
    P.getPosition
        |> P.andThen
            (\end ->
                P.oneOfWithFallback
                    [ Space.checkIndent end E.DeclStart
                        |> P.andThen (\_ -> Type.term)
                        |> P.andThen
                            (\( arg, newEnd ) ->
                                Space.chomp E.DeclSpace
                                    |> P.andThen (\_ -> chompVariantArgs (arg :: revArgs))
                            )
                    ]
                    ( List.reverse revArgs, end )
            )



-- PORT DECLARATIONS


portDecl : Position -> Parser E.Problem ( Decl, Position )
portDecl start =
    Keyword.port_ E.DeclStart
        |> P.andThen
            (\_ ->
                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                    |> P.andThen
                        (\_ ->
                            P.addLocation (Var.lower E.DeclStart)
                                |> P.andThen
                                    (\name ->
                                        Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                            |> P.andThen
                                                (\_ ->
                                                    P.symbol ":" E.DeclStart
                                                        |> P.andThen
                                                            (\_ ->
                                                                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                                                    |> P.andThen (\_ -> Type.expression)
                                                                    |> P.andThen
                                                                        (\( tipe, end ) ->
                                                                            let
                                                                                port_ =
                                                                                    Src.Port name tipe
                                                                            in
                                                                            P.succeed ( PortDecl port_, end )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )



-- VALUE DECLARATIONS


valueDecl : Position -> Parser E.Problem ( Decl, Position )
valueDecl start =
    P.addLocation (Var.lower E.DeclStart)
        |> P.andThen
            (\((Src.At _ funcName) as locName) ->
                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                    |> P.andThen
                        (\_ ->
                            P.oneOf E.DeclStart
                                [ -- Type annotation followed by definition
                                  P.symbol ":" E.DeclStart
                                    |> P.andThen
                                        (\_ ->
                                            Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                                |> P.andThen (\_ -> Type.expression)
                                                |> P.andThen
                                                    (\( tipe, _ ) ->
                                                        Space.checkFreshLine E.DeclStart
                                                            |> P.andThen
                                                                (\_ ->
                                                                    chompMatchingName funcName
                                                                        |> P.andThen
                                                                            (\defName ->
                                                                                Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                                                                    |> P.andThen (\_ -> chompValueBody start defName (Just tipe))
                                                                            )
                                                                )
                                                    )
                                        )
                                , -- Just definition (no type annotation)
                                  chompValueBody start locName Nothing
                                ]
                        )
            )


chompValueBody : Position -> Src.Located String -> Maybe Src.Type -> Parser E.Problem ( Decl, Position )
chompValueBody start name tipe =
    chompArgs []
        |> P.andThen
            (\revArgs ->
                P.symbol "=" E.DeclStart
                    |> P.andThen
                        (\_ ->
                            Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                                |> P.andThen (\_ -> Expr.expression)
                                |> P.andThen
                                    (\( body, end ) ->
                                        let
                                            value =
                                                Src.Value name (List.reverse revArgs) body tipe
                                        in
                                        P.succeed
                                            ( Value (Src.at start end value)
                                            , end
                                            )
                                    )
                        )
            )


chompArgs : List Src.Pattern -> Parser E.Problem (List Src.Pattern)
chompArgs revArgs =
    P.oneOfWithFallback
        [ Pattern.term
            |> P.andThen
                (\arg ->
                    Space.chompAndCheckIndent E.DeclSpace E.DeclStart
                        |> P.andThen (\_ -> chompArgs (arg :: revArgs))
                )
        ]
        (List.reverse revArgs)


chompMatchingName : String -> Parser E.Problem (Src.Located String)
chompMatchingName expectedName =
    P.addLocation (Var.lower E.DeclStart)
        |> P.andThen
            (\((Src.At _ name) as locatedName) ->
                if name == expectedName then
                    P.succeed locatedName

                else
                    P.fail E.DeclStart
            )
