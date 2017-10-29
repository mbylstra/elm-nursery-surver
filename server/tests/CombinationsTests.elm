module CombinationsTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Combinations exposing (combinations, foldFunction)


suite : Test
suite =
    describe "Combinations.elm"
        [ describe "foldFunction"
            [ test "foldFunction [2] [[1]]" <|
                \_ ->
                    foldFunction
                        [ 2 ]
                        [ [ 1 ] ]
                        |> Expect.equal
                            [ [ 1, 2 ]
                            ]
            ]
        , describe "combinations"
            [ test "combinations [1] [2,3]" <|
                \_ ->
                    combinations
                        [ [ 1 ]
                        , [ 2, 3 ]
                        ]
                        |> Expect.equal
                            [ [ 1, 2 ]
                            , [ 1, 3 ]
                            ]
            , test "combinations [1,2] [3,4]" <|
                \_ ->
                    combinations
                        [ [ 1, 2 ]
                        , [ 3, 4 ]
                        ]
                        |> Expect.equal
                            [ [ 1, 3 ]
                            , [ 2, 3 ]
                            , [ 1, 4 ]
                            , [ 2, 4 ]
                            ]
            , test "combinations [1,2] [3,4] [5,6]" <|
                \_ ->
                    combinations
                        [ [ 1, 2 ]
                        , [ 3, 4 ]
                        , [ 5, 6 ]
                        ]
                        |> Expect.equal
                            [ [ 1, 3, 5 ]
                            , [ 2, 3, 5 ]
                            , [ 1, 4, 5 ]
                            , [ 2, 4, 5 ]
                            , [ 1, 3, 6 ]
                            , [ 2, 3, 6 ]
                            , [ 1, 4, 6 ]
                            , [ 2, 4, 6 ]
                            ]
            ]
        ]
