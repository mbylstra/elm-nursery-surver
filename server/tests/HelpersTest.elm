module HelpersTest exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Helpers exposing (qualifiedNameToPath, groupByFirstTupleItem)
import Dict
import Result


suite : Test
suite =
    describe "Helpers.elm"
        [ describe "qualifiedNameToPath"
            [ test "Foo.Bar.Baz" <|
                \_ ->
                    "Foo.Bar.Baz"
                        |> qualifiedNameToPath
                        |> Expect.equal
                            "Foo/Bar/Baz.elm"
            , test "Foo" <|
                \_ ->
                    "Foo"
                        |> qualifiedNameToPath
                        |> Expect.equal
                            "Foo.elm"
            ]
        , describe "groupBy"
            [ test "creates a list of two if two keys are the same" <|
                \_ ->
                    [ ( "a", 1 ), ( "a", 2 ) ]
                        |> groupByFirstTupleItem
                        |> Expect.equal
                            (Dict.fromList [ ( "a", [ 1, 2 ] ) ])
            , test "creates two lists if two keys are different" <|
                \_ ->
                    [ ( "a", 1 ), ( "b", 2 ) ]
                        |> groupByFirstTupleItem
                        |> Expect.equal
                            (Dict.fromList [ ( "a", [ 1 ] ), ( "b", [ 2 ] ) ])
            ]
        , describe "anyTrue"
            [ test "return true if at least on is true" <|
                \_ ->
                    [ False, True, False ]
                        |> Helpers.anyTrue
                        |> Expect.equal True
            , test "return true if all true" <|
                \_ ->
                    [ True, True, True ]
                        |> Helpers.anyTrue
                        |> Expect.equal True
            , test "return false if none true" <|
                \_ ->
                    [ False, False, False ]
                        |> Helpers.anyTrue
                        |> Expect.equal False
            ]
        , describe "allTrue"
            [ test "return false if not all are true" <|
                \_ ->
                    [ True, True, False ]
                        |> Helpers.allTrue
                        |> Expect.equal False
            , test "return true if all are true" <|
                \_ ->
                    [ True, True, True ]
                        |> Helpers.allTrue
                        |> Expect.equal True
            , test "return false if none are true" <|
                \_ ->
                    [ False, False, False ]
                        |> Helpers.allTrue
                        |> Expect.equal False
            ]
        , describe "removeDuplicates"
            [ test "removes duplicates and preserves order" <|
                \_ ->
                    [ 1, 2, 2, 2, 3, 2, 3 ]
                        |> Helpers.removeDuplicates
                        |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "unsafeDictGet"
            [ test "removes duplicates and preserves order" <|
                \_ ->
                    Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]
                        |> Helpers.unsafeDictGet "error" 1
                        |> Expect.equal "a"
            ]
        , describe "unsafeAssumeSuccess"
            [ test "unwraps an Ok value" <|
                \_ ->
                    Helpers.unsafeAssumeSuccess (Result.Ok "hello")
                        |> Expect.equal "hello"
            ]
        , describe "unsafeListHead"
            [ test "unwraps successful result" <|
                \_ ->
                    Helpers.unsafeListHead [ "a" ]
                        |> Expect.equal "a"
            ]
        ]
