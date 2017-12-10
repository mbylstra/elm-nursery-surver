module FirstPassTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import FirstPass
    exposing
        ( parseModule
        , removeOneLineComments
        , splitIntoBlocks
        , classifyBlocks
        )


suite : Test
suite =
    describe "FirstPass.elm"
        [ describe "parseModule"
            [ test "basic test" <|
                \_ ->
                    ([ "module Main exposing (..)"
                     , "type Result error value = Ok value | Err error"
                     ]
                        |> String.join "\n"
                    )
                        |> String.lines
                        |> removeOneLineComments
                        |> splitIntoBlocks
                        |> classifyBlocks
                        |> Expect.equal
                            [ FirstPass.ModuleStatementBlock "module Main exposing (..)"
                            , FirstPass.TypeDefinition "type Result error value = Ok value | Err error"
                            ]
            , test "test with multi line comments" <|
                \_ ->
                    ([ "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
                     , "`Err` meaning that there was some failure."
                     , "-}"
                     , "type Result error value = Ok value | Err error"
                     ]
                        |> String.join "\n"
                    )
                        |> String.lines
                        |> removeOneLineComments
                        |> splitIntoBlocks
                        |> classifyBlocks
                        |> Expect.equal
                            [ FirstPass.EmptyLines "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
                            , FirstPass.EmptyLines "`Err` meaning that there was some failure."
                            , FirstPass.EmptyLines "-}"
                            , FirstPass.TypeDefinition "type Result error value = Ok value | Err error"
                            ]
            ]

        -- This is basically too annoying to test - we just want to know that right functions are
        -- called. The functions themselves are already tested.
        -- , describe "parseBlock"
        --
        --     [ test "EmptyLines" <|
        --         \_ ->
        --             FirstPass.parseBlock (FirstPass.EmptyLines "\n\n")
        --                 |> Expect.equal (Ok Types.IgnoreBlock)
        --     , test "ImportStatementBlock" <|
        --         \_ ->
        --             FirstPass.parseBlock (FirstPass.ImportStatementBlock "\n\n")
        --                 |> Expect.equal (Ok Types.Import)
        --     ]
        , describe "replaceNewLinesWithSpaces"
            [ test "that they are replaced with spaces :)" <|
                \_ ->
                    FirstPass.replaceNewLinesWithSpaces (FirstPass.replaceNewLinesWithSpaces "one\ntwo\n")
                        |> Expect.equal "one two "
            ]
        ]
