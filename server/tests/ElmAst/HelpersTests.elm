module ElmAst.HelpersTests exposing (..)

import ElmAst.Helpers exposing (someWhitespace)
import Expect exposing (Expectation, equalSets)
import Parser exposing (Parser, (|.), (|=))
import Result.Extra exposing (isErr)
import Test exposing (..)


suite : Test
suite =
    describe "ParserHelpers"
        [ test "someWhitespace 1" <|
            \_ ->
                ""
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal True
        , test "someWhitespace 2" <|
            \_ ->
                "\n"
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal False
        , test "someWhitespace 3" <|
            \_ ->
                "\n\n"
                    |> Parser.run someWhitespace
                    |> isErr
                    |> Expect.equal False

        -- , test "someWhitespace 3" <|
        --     \_ ->
        --         "\n --comment"
        --             |> Parser.run someWhitespace
        --             |> isErr
        --             |> Expect.equal False
        -- , test "someWhitespace 4" <|
        --     -- This is expected, but it should be improved so that a comment counts as whitespace
        --     \_ ->
        --         "--comment"
        --             |> Parser.run someWhitespace
        --             |> isErr
        --             |> Expect.equal True
        ]
