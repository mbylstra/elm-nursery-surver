module ElmAst.NameTests exposing (..)

import ElmAst.Name as Name
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Parser exposing (Parser, (|.), (|=))
import Result.Extra exposing (isErr)
import Test exposing (..)


suite : Test
suite =
    describe "ElmAst.Name"
        [ describe "lowerCaseName"
            [ test "it passes" <|
                \_ ->
                    "foo"
                        |> Parser.run Name.lowerCaseName
                        |> Expect.equal
                            (Ok "foo")
            , test "it fails" <|
                \_ ->
                    "Foo"
                        |> Parser.run Name.lowerCaseName
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "capitalizedName"
            [ test "it passes" <|
                \_ ->
                    "Foo"
                        |> Parser.run Name.capitalizedName
                        |> Expect.equal
                            (Ok "Foo")
            , test "it fails" <|
                \_ ->
                    "foo"
                        |> Parser.run Name.capitalizedName
                        |> isErr
                        |> Expect.equal True
            ]
        , describe "qualifiedCapitalizedName"
            [ test "it passes" <|
                \_ ->
                    "Foo.Bar"
                        |> Parser.run Name.qualifiedCapitalizedName
                        |> Expect.equal
                            (Ok "Foo.Bar")
            , test "not qualified passes" <|
                \_ ->
                    "Foo"
                        |> Parser.run Name.qualifiedCapitalizedName
                        |> Expect.equal
                            (Ok "Foo")
            , test "it fails" <|
                \_ ->
                    "Foo.bar"
                        |> Parser.run Name.qualifiedCapitalizedName
                        |> isErr
                        |> Expect.equal True
            ]
        ]
