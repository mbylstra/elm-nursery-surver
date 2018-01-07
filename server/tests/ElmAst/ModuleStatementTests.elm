module ElmAst.ModuleStatementTests exposing (..)

import ElmAst.ModuleStatement
import Expect exposing (Expectation, equalSets)
import Test exposing (..)


suite : Test
suite =
    describe "ElmAst.ModuleStatement"
        [ test "basic Main" <|
            \_ ->
                "module Main exposing (..)"
                    |> ElmAst.ModuleStatement.parseModuleStatement
                    |> Expect.equal (Ok <| "Main")
        , test "exposing stuff" <|
            \_ ->
                "module Main exposing (Foo,Bar)"
                    |> ElmAst.ModuleStatement.parseModuleStatement
                    |> Expect.equal (Ok <| "Main")
        , test "port module" <|
            \_ ->
                "port module Main exposing (Foo,Bar)"
                    |> ElmAst.ModuleStatement.parseModuleStatement
                    |> Expect.equal (Ok <| "Main")
        ]
