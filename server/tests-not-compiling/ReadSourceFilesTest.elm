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
                                    , ( "dir8", DirNotAttemptedYet )
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
        , describe "update"
            [ test "sets source code when source code found" <|
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

                        msg =
                            ReadSourceFiles.ReadElmModuleResult
                                { maybeContents = Just "x = 1"
                                , portScope = { path = "dir1", dir = "dir1", moduleName = "Module1" }
                                }
                    in
                        ReadSourceFiles.update msg False model
                            |> Expect.equal
                                { rsfModel =
                                    { moduleName = "Module1"
                                    , maybeSourceCode = Just "x = 1"
                                    , dirAttempts =
                                        Dict.fromList
                                            [ ( "dir1", DirSuccess )
                                            ]
                                    }
                                , rsfGoal = Just "x = 1"
                                , rsfCmds = []
                                }
            , test "sets failure when wrong dir looked up" <|
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

                        msg =
                            ReadSourceFiles.ReadElmModuleResult
                                { maybeContents = Nothing
                                , portScope = { path = "dir1", dir = "dir1", moduleName = "Module1" }
                                }
                    in
                        ReadSourceFiles.update msg False model
                            |> Expect.equal
                                { rsfModel =
                                    { moduleName = "Module1"
                                    , maybeSourceCode = Nothing
                                    , dirAttempts =
                                        Dict.fromList
                                            [ ( "dir1", DirFail )
                                            ]
                                    }
                                , rsfGoal = Nothing
                                , rsfCmds = []
                                }
            , test "emits a cmd and sets InFlight if there is still a dir to try" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                            }

                        msg =
                            ReadSourceFiles.ReadElmModuleResult
                                { maybeContents = Nothing
                                , portScope = { path = "dir1", dir = "dir1", moduleName = "Module1" }
                                }
                    in
                        ReadSourceFiles.update msg False model
                            |> Expect.all
                                [ .rsfGoal >> Expect.equal Nothing
                                , .rsfModel
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , maybeSourceCode = Nothing
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", DirFail )
                                                , ( "dir2", InFlight )
                                                ]
                                        }
                                , .rsfCmds >> List.length >> Expect.equal 1
                                ]
            , test "don't bother trying more dirs if we already found one" <|
                \_ ->
                    let
                        model =
                            { moduleName = "Module1"
                            , maybeSourceCode = Nothing
                            , dirAttempts =
                                Dict.fromList
                                    [ ( "dir1", InFlight )
                                    , ( "dir2", DirNotAttemptedYet )
                                    ]
                            }

                        msg =
                            ReadSourceFiles.ReadElmModuleResult
                                { maybeContents = Just "x = 1"
                                , portScope = { path = "dir1", dir = "dir1", moduleName = "Module1" }
                                }
                    in
                        ReadSourceFiles.update msg False model
                            |> Expect.all
                                [ .rsfGoal >> Expect.equal (Just "x = 1")
                                , .rsfModel
                                    >> Expect.equal
                                        { moduleName = "Module1"
                                        , maybeSourceCode = Just "x = 1"
                                        , dirAttempts =
                                            Dict.fromList
                                                [ ( "dir1", DirSuccess )
                                                , ( "dir2", DirNotAttemptedYet )
                                                ]
                                        }
                                , .rsfCmds >> List.length >> Expect.equal 0
                                ]
            ]
        , describe "updateDirAttempt"
            [ test "sets to success if there is source code" <|
                \_ ->
                    ReadSourceFiles.updateDirAttempt (Just "x = 1") InFlight
                        |> Expect.equal DirSuccess
            , test "sets to fail if not" <|
                \_ ->
                    ReadSourceFiles.updateDirAttempt Nothing InFlight
                        |> Expect.equal DirFail
            ]
        , describe "isInFlight"
            [ test "InFlight returns True" <|
                \_ ->
                    ReadSourceFiles.isInFlight InFlight
                        |> Expect.equal True
            , test "DirSuccess returns False" <|
                \_ ->
                    ReadSourceFiles.isInFlight DirSuccess
                        |> Expect.equal False
            , test "DirFail returns False" <|
                \_ ->
                    ReadSourceFiles.isInFlight DirFail
                        |> Expect.equal False
            , test "DirNotAttemptedYet returns False" <|
                \_ ->
                    ReadSourceFiles.isInFlight DirNotAttemptedYet
                        |> Expect.equal False
            ]
        , describe "numCmdsInFlight"
            [ test "returns 6 if 6 are in flight" <|
                \_ ->
                    let
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
                                    , ( "dir8", DirNotAttemptedYet )
                                    ]
                            }
                    in
                        ReadSourceFiles.numCmdsInFlight model
                            |> Expect.equal 6
            ]
        ]
