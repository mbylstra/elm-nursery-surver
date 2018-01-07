module ElmAst.ImportStatementTests exposing (..)

import ElmAst.ImportStatement exposing (openListing, closedListing, listing)
import Expect exposing (Expectation, equalSets)
import Test exposing (..)


suite : Test
suite =
    describe "ElmAst.ImportStatement"
        [ test "import Foo" <|
            \_ ->
                "import Foo"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = closedListing
                            }
                        )
        , test "import Foo as Bar" <|
            \_ ->
                "import Foo as Bar"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Just "Bar"
                            , exposedNames = closedListing
                            }
                        )
        , test "import Foo exposing (..)" <|
            \_ ->
                "import Foo exposing (..)"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = openListing
                            }
                        )
        , test "import Foo as Bar exposing (..)" <|
            \_ ->
                "import Foo as Bar exposing (..)"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Just "Bar"
                            , exposedNames = openListing
                            }
                        )
        , test "import Foo exposing (Bar)" <|
            \_ ->
                "import Foo exposing (Bar)"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar" ]
                            }
                        )
        , test "import Foo exposing (Bar,Baz)" <|
            \_ ->
                "import Foo exposing (Bar,Baz)"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar", "Baz" ]
                            }
                        )
        , test "import Foo exposing (Bar(..))" <|
            \_ ->
                "import Foo exposing (Bar(..))"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar" ]
                            }
                        )
        , test "import Foo exposing (Bar(..),Baz)" <|
            \_ ->
                "import Foo exposing (Bar(..),Baz)"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar", "Baz" ]
                            }
                        )
        , test "import Foo exposing (Bar(Baz))" <|
            \_ ->
                "import Foo exposing (Bar(Baz))"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar" ]
                            }
                        )
        , test "import Foo exposing (Bar(Baz,Qux))" <|
            \_ ->
                "import Foo exposing (Bar(Baz,Qux))"
                    |> ElmAst.ImportStatement.parseImportStatement
                    |> Expect.equal
                        (Ok <|
                            { dottedModulePath = "Foo"
                            , maybeAlias = Nothing
                            , exposedNames = listing [ "Bar" ]
                            }
                        )
        ]
