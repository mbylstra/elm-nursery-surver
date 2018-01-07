module ElmAst.HelpersTests exposing (..)

import ElmAst.Helpers exposing (someWhitespace, oneOrMoreSeparatedByComma)
import Expect exposing (Expectation, equalSets)
import Parser exposing (Parser, (|.), (|=))
import Result.Extra exposing (isErr)
import Test exposing (..)


suite : Test
suite =
    describe "ParserHelpers"
        [ describe "someWhitespace"
            [ test "none" <|
                \_ ->
                    ""
                        |> Parser.run someWhitespace
                        |> isErr
                        |> Expect.equal True
            , test "one" <|
                \_ ->
                    "\n"
                        |> Parser.run someWhitespace
                        |> isErr
                        |> Expect.equal False
            , test "two" <|
                \_ ->
                    "\n\n"
                        |> Parser.run someWhitespace
                        |> isErr
                        |> Expect.equal False
            ]
        , describe "oneOrMoreSeparatedByComma"
            [ test "just one" <|
                \_ ->
                    "x"
                        |> Parser.run (oneOrMoreSeparatedByComma <| Parser.symbol "x")
                        |> isErr
                        |> Expect.equal False
            , test "two" <|
                \_ ->
                    "x,x"
                        |> Parser.run (oneOrMoreSeparatedByComma <| Parser.symbol "x")
                        |> isErr
                        |> Expect.equal False
            , test "three" <|
                \_ ->
                    "x,x,x"
                        |> Parser.run (oneOrMoreSeparatedByComma <| Parser.symbol "x")
                        |> isErr
                        |> Expect.equal False
            , test "fails" <|
                \_ ->
                    "x,x,x,"
                        |> Parser.run (oneOrMoreSeparatedByComma <| Parser.symbol "x")
                        |> isErr
                        |> Expect.equal True
            ]
        ]
