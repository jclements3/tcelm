module Format.Test exposing (main, testFormat)

{-| Test module for the Elm formatter.

Run this to see example formatted output.

-}

import AST.Source as Src exposing (Located(..))
import Format.Format as Format
import Platform


main : Program () () ()
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


{-| Test formatting a sample module.
-}
testFormat : String
testFormat =
    let
        -- Create a sample module AST
        sampleModule : Src.Module
        sampleModule =
            { name = Just (At dummyRegion "Example")
            , exports = At dummyRegion Src.Open
            , imports = []
            , values =
                [ At dummyRegion
                    { name = At dummyRegion "person"
                    , args = []
                    , body =
                        At dummyRegion
                            (Src.Record
                                [ ( At dummyRegion "firstName", At dummyRegion (Src.Str "John") )
                                , ( At dummyRegion "lastName", At dummyRegion (Src.Str "Doe") )
                                , ( At dummyRegion "age", At dummyRegion (Src.Int 30) )
                                ]
                            )
                    , type_ = Nothing
                    }
                , At dummyRegion
                    { name = At dummyRegion "config"
                    , args = []
                    , body =
                        At dummyRegion
                            (Src.Record
                                [ ( At dummyRegion "host", At dummyRegion (Src.Str "localhost") )
                                , ( At dummyRegion "port_", At dummyRegion (Src.Int 8080) )
                                , ( At dummyRegion "debug", At dummyRegion (Src.Var Src.CapVar "True") )
                                , ( At dummyRegion "logLevel", At dummyRegion (Src.Str "info") )
                                ]
                            )
                    , type_ = Nothing
                    }
                ]
            , unions =
                [ At dummyRegion
                    { name = At dummyRegion "Msg"
                    , args = []
                    , ctors =
                        [ ( At dummyRegion "Click", [] )
                        , ( At dummyRegion "Hover", [ At dummyRegion (Src.TType dummyRegion "String" []) ] )
                        , ( At dummyRegion "KeyPress", [ At dummyRegion (Src.TType dummyRegion "Char" []) ] )
                        , ( At dummyRegion "MouseMove"
                          , [ At dummyRegion (Src.TType dummyRegion "Int" [])
                            , At dummyRegion (Src.TType dummyRegion "Int" [])
                            ]
                          )
                        ]
                    }
                ]
            , aliases =
                [ At dummyRegion
                    { name = At dummyRegion "Person"
                    , args = []
                    , type_ =
                        At dummyRegion
                            (Src.TRecord
                                [ ( At dummyRegion "firstName", At dummyRegion (Src.TType dummyRegion "String" []) )
                                , ( At dummyRegion "lastName", At dummyRegion (Src.TType dummyRegion "String" []) )
                                , ( At dummyRegion "age", At dummyRegion (Src.TType dummyRegion "Int" []) )
                                ]
                                Nothing
                            )
                    }
                ]
            , binops = []
            , ports = []
            }
    in
    Format.format sampleModule


dummyRegion : Src.Region
dummyRegion =
    { start = { row = 1, col = 1 }
    , end = { row = 1, col = 1 }
    }
