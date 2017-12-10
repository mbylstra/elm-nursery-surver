module ReadSourceFilesTest exposing (..)

import Dict
import Expect exposing (Expectation, equalSets)
import ReadSourceFiles
    exposing
        ( DirAttempt(DirFail, DirNotAttemptedYet, DirSuccess, InFlight)
        , Model
        , atLeastOneSuccess
        , haveNotExhaustedAllOptions
        , getNextCmds
        , getGoal
        , readElmModule
        , init
        , getNextCmdsForDirAttempts
        )
import Test exposing (..)


suite : Test
suite =
    describe "ReadSourceFiles.elm"
        [ describe "init"
            (let
                initArg =
                    { moduleName = "Foo", sourceDirectories = [ "dir1", "dir2" ] }
             in
                [ test "model" <|
                    \_ ->
                        let
                            ( model, _ ) =
                                init initArg
                        in
                            model
                                |> Expect.equal
                                    { moduleName = "Foo"
                                    , maybeSourceCode = Nothing
                                    , dirAttempts =
                                        Dict.fromList
                                            [ ( "dir1", InFlight )
                                            , ( "dir2", DirNotAttemptedYet )
                                            ]
                                    }
                , test "cmd" <|
                    \_ ->
                        let
                            ( _, maybeCmd ) =
                                init initArg
                        in
                            maybeCmd
                                |> Expect.equal
                                    [ readElmModule
                                        { path = "dir1/Foo.elm"
                                        , portScope =
                                            { path = "dir1/Foo.elm"
                                            , dir = "dir1"
                                            , moduleName = "Foo"
                                            }
                                        }
                                    ]
                ]
            )
        , describe "kickBackIntoAction"
            [ test "should output a command if not finished" <|
                \_ ->
                    let
                        model : Model
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            ReadSourceFiles.kickBackIntoAction model
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , maybeSourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", InFlight )
                                                ]
                                        }
                                , Tuple.second >> (Expect.notEqual [])
                                ]
            ]
        , describe "getNextCmds"
            [ test "it returns cmds if max commands not reached" <|
                \_ ->
                    let
                        model : Model
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            getNextCmds { model = model, maxCmdsReached = False }
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , maybeSourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", InFlight )
                                                ]
                                        }
                                , Tuple.second >> (Expect.notEqual [])
                                ]
            , test "it does not return cmds if max cmds reached" <|
                \_ ->
                    let
                        model : Model
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            getNextCmds { model = model, maxCmdsReached = True }
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal model
                                , Tuple.second >> (Expect.equal [])
                                ]
            , test "it does not return cmds if more than 5 requests in flight" <|
                \_ ->
                    let
                        model : Model
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", InFlight )
                                    , ( "dir3", InFlight )
                                    , ( "dir4", InFlight )
                                    , ( "dir5", InFlight )
                                    , ( "dir6", InFlight )
                                    , ( "dir7", DirNotAttemptedYet )
                                    ]
                            }

                        result =
                            getNextCmds { model = model, maxCmdsReached = False }
                    in
                        result
                            |> Expect.all
                                [ Tuple.first
                                    >> Expect.equal model
                                , Tuple.second >> (Expect.equal [])
                                ]
            ]
        , describe "getNextCmdForDirAttempts"
            (let
                dirAttempts =
                    Dict.fromList
                        [ ( "dir1", DirNotAttemptedYet )
                        , ( "dir2", DirNotAttemptedYet )
                        ]
             in
                [ test "none attempted yet" <|
                    \_ ->
                        getNextCmdsForDirAttempts "Foo" dirAttempts
                            |> Expect.equal
                                ( Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                                , [ readElmModule
                                        { path = "dir1/Foo.elm"
                                        , portScope =
                                            { path = "dir1/Foo.elm"
                                            , dir = "dir1"
                                            , moduleName = "Foo"
                                            }
                                        }
                                  ]
                                )
                , test "one in flight" <|
                    \_ ->
                        let
                            dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                        in
                            getNextCmdsForDirAttempts "Foo" dirAttempts
                                |> Expect.equal
                                    ( Dict.fromList
                                        [ ( "dir1", InFlight )
                                        , ( "dir2", InFlight )
                                        ]
                                    , [ readElmModule
                                            { path = "dir2/Foo.elm"
                                            , portScope =
                                                { path = "dir2/Foo.elm"
                                                , dir = "dir2"
                                                , moduleName = "Foo"
                                                }
                                            }
                                      ]
                                    )
                ]
            )
        , describe "getGoal"
            [ test "returns Err if not finished" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts = Dict.fromList [ ( "dir1", DirNotAttemptedYet ) ]
                            }
                    in
                        getGoal model
                            |> Expect.equal
                                (Err model)
            , test "returns Ok with source code if finished" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Just "x = 1"
                            , dirAttempts = Dict.fromList [ ( "dir1", DirSuccess ) ]
                            }
                    in
                        getGoal model
                            |> Expect.equal
                                (Ok "x = 1")
            ]
        , describe "isFinished"
            [ test "returns true if finished" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Just "x = 1"
                            , dirAttempts = Dict.fromList [ ( "dir1", DirSuccess ) ]
                            }
                    in
                        ReadSourceFiles.isFinished model
                            |> Expect.equal True
            , test "returns false if  not finished" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts = Dict.fromList [ ( "dir1", DirSuccess ), ( "dir2", InFlight ) ]
                            }
                    in
                        ReadSourceFiles.isFinished model
                            |> Expect.equal False
            ]
        , describe "atLeastOneSuccess"
            [ test "returns a dir wrapped in Just if there's at least one success" <|
                \_ ->
                    (Dict.fromList
                        [ ( "dir1", DirFail )
                        , ( "dir2", DirSuccess )
                        , ( "dir3", DirSuccess )
                        ]
                    )
                        |> atLeastOneSuccess
                        |> Expect.equal (Just "dir2")
            , test "returns Nothing if no successes" <|
                \_ ->
                    (Dict.fromList
                        [ ( "dir1", DirFail )
                        , ( "dir2", InFlight )
                        ]
                    )
                        |> atLeastOneSuccess
                        |> Expect.equal Nothing
            ]
        , describe "haveNotExhaustedAllOptions" <|
            [ test "returns a dir wrapped in Just if not all options have been exhausted (DirNotAttempted)" <|
                \_ ->
                    (Dict.fromList
                        [ ( "dir1", DirNotAttemptedYet )
                        ]
                    )
                        |> haveNotExhaustedAllOptions
                        |> Expect.equal (Just "dir1")
            , test "returns a dir wrapped in Just if not all options have been exhausted (InFlight)" <|
                \_ ->
                    (Dict.fromList
                        [ ( "dir1", InFlight )
                        ]
                    )
                        |> haveNotExhaustedAllOptions
                        |> Expect.equal (Just "dir1")
            , test "returns Nothing if all options have been exhausted" <|
                \_ ->
                    (Dict.fromList
                        [ ( "dir1", DirFail )
                        , ( "dir2", DirFail )
                        ]
                    )
                        |> haveNotExhaustedAllOptions
                        |> Expect.equal Nothing
            ]
        , describe "hasFailed" <|
            [ test "returns Nothing there's source Code" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Just "x = 1"
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirFail )
                                    , ( "dir2", DirFail )
                                    ]
                            }
                    in
                        ReadSourceFiles.hasFailed model
                            |> Expect.equal Nothing
            , test "returns Nothing if haven't exhausted all options (DirNotAttempted)" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirNotAttemptedYet )
                                    ]
                            }
                    in
                        ReadSourceFiles.hasFailed model
                            |> Expect.equal Nothing
            , test "returns Nothing if haven't exhausted all options (InFlight)" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    ]
                            }
                    in
                        ReadSourceFiles.hasFailed model
                            |> Expect.equal Nothing
            , test "returns Just model if it failed" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", DirFail )
                                    , ( "dir2", DirFail )
                                    ]
                            }
                    in
                        ReadSourceFiles.hasFailed model
                            |> Expect.equal (Just model)
            ]
        ]
