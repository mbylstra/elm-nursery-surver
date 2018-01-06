module ElmAst.VarTests exposing (..)

import ElmAst.Var as Var
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Parser exposing (Parser, (|.), (|=))
import Result.Extra exposing (isErr)
import Test exposing (..)


suite : Test
suite =
    describe "ElmAst.Var"
        [ describe "lowVar"
            [ test "it passes" <|
                \_ ->
                    "foo"
                        |> Parser.run Var.lowVar
                        |> Expect.equal
                            (Ok "foo")
            , test "it fails" <|
                \_ ->
                    "Foo"
                        |> Parser.run Var.lowVar
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "capVar"
            [ test "it passes" <|
                \_ ->
                    "Foo"
                        |> Parser.run Var.capVar
                        |> Expect.equal
                            (Ok "Foo")
            , test "it fails" <|
                \_ ->
                    "foo"
                        |> Parser.run Var.capVar
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "qualifiedCapVar"
            [ test "it passes" <|
                \_ ->
                    "Foo.Bar"
                        |> Parser.run Var.qualifiedCapVar
                        |> Expect.equal
                            (Ok "Foo.Bar")
            , test "not qualified passes" <|
                \_ ->
                    "Foo"
                        |> Parser.run Var.qualifiedCapVar
                        |> Expect.equal
                            (Ok "Foo")
            , test "it fails" <|
                \_ ->
                    "Foo.bar"
                        |> Parser.run Var.qualifiedCapVar
                        |> isErr
                        |> Expect.equal True
            ]
        ]
