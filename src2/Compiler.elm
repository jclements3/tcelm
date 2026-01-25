port module Compiler exposing (main)

{-| Main entry point for tcelm v2.

This is the new architecture with:
- Proper type inference
- Type classes support
- Core IR intermediate representation
- Modular compilation pipeline
-}

import Platform
import AST
import Parser
import Infer
import Types exposing (Type(..), Kind(..))
import Desugar
import Core
import Codegen.C as C


-- MAIN


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { source : String
    , output : String
    , errors : List String
    }


type alias Flags =
    { source : String
    , target : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        result = compile flags.source flags.target
    in
    case result of
        Ok output ->
            ( { source = flags.source
              , output = output
              , errors = []
              }
            , printOutput output
            )

        Err errors ->
            ( { source = flags.source
              , output = ""
              , errors = errors
              }
            , printErrors errors
            )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- COMPILATION PIPELINE


{-| The main compilation pipeline:

    Source -> Parse -> Type Check -> Desugar -> Core IR -> Codegen -> C
-}
compile : String -> String -> Result (List String) String
compile source target =
    -- Step 1: Parse
    case Parser.parse source of
        Err parseError ->
            Err [ "Parse error: " ++ parseError.message
                    ++ " at line " ++ String.fromInt parseError.region.start.line
                    ++ ", column " ++ String.fromInt parseError.region.start.column
                ]

        Ok ast ->
            -- Step 2: Type check
            case Infer.inferModule Infer.emptyEnv ast of
                Err typeError ->
                    Err [ "Type error: " ++ typeErrorToString typeError ]

                Ok _ ->
                    -- Step 3: Desugar to Core
                    let
                        coreModule = Desugar.desugarModule ast
                    in
                    -- Step 4: Generate C code
                    let
                        defaultOpts = C.defaultOptions
                        options =
                            { defaultOpts | targetName = target }

                        cCode = C.generateC options coreModule
                    in
                    Ok cCode


typeErrorToString : Infer.TypeError -> String
typeErrorToString err =
    case err of
        Infer.UnificationError t1 t2 ->
            "Cannot unify " ++ typeToString t1 ++ " with " ++ typeToString t2

        Infer.OccursCheck v t ->
            "Infinite type: " ++ v ++ " occurs in " ++ typeToString t

        Infer.UnboundVariable name ->
            "Unbound variable: " ++ name

        Infer.UnboundConstructor name ->
            "Unbound constructor: " ++ name

        Infer.UnboundTypeClass name ->
            "Unbound type class: " ++ name

        Infer.NoInstance cls ty ->
            "No instance of " ++ cls ++ " for " ++ typeToString ty

        Infer.KindMismatch k1 k2 ->
            "Kind mismatch: " ++ kindToString k1 ++ " vs " ++ kindToString k2

        Infer.ArityMismatch name expected actual ->
            name ++ " expects " ++ String.fromInt expected
                ++ " arguments but got " ++ String.fromInt actual

        Infer.NotAFunction t ->
            "Not a function: " ++ typeToString t

        Infer.RecordFieldMissing field ->
            "Missing record field: " ++ field

        Infer.DuplicateDefinition name ->
            "Duplicate definition: " ++ name

        Infer.Other msg ->
            msg


typeToString : Type -> String
typeToString ty =
    case ty of
        TVar v ->
            v

        TCon name ->
            name

        TApp t1 t2 ->
            typeToString t1 ++ " " ++ parenTypeToString t2

        TArrow t1 t2 ->
            parenTypeToString t1 ++ " -> " ++ typeToString t2

        TRecord fields _ ->
            "{ " ++ String.join ", " (List.map (\( n, t ) -> n ++ " : " ++ typeToString t) fields) ++ " }"

        TTuple types ->
            "(" ++ String.join ", " (List.map typeToString types) ++ ")"

        TForall vars _ body ->
            "forall " ++ String.join " " vars ++ ". " ++ typeToString body


parenTypeToString : Type -> String
parenTypeToString ty =
    case ty of
        TApp _ _ ->
            "(" ++ typeToString ty ++ ")"

        TArrow _ _ ->
            "(" ++ typeToString ty ++ ")"

        _ ->
            typeToString ty


kindToString : Kind -> String
kindToString k =
    case k of
        KStar ->
            "*"

        KArrow k1 k2 ->
            kindToString k1 ++ " -> " ++ kindToString k2

        KConstraint ->
            "Constraint"



-- PORTS (for Node.js interaction)


port printOutput : String -> Cmd msg
port printErrors : List String -> Cmd msg
