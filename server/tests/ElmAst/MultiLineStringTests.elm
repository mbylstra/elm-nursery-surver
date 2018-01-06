module ElmAst.MultiLineStringTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import ElmAst.MultiLineString


suite : Test
suite =
    describe "ElmAst.MultiLineString.elm"
        [ describe "removeMultiLineComments"
            [ test "works on a simple case" <|
                \_ ->
                    "outside\noutside\"\"\"inside\ninside\"\"\"outside\noutside"
                        |> ElmAst.MultiLineString.convertMultiLineStrings
                        |> Expect.equal
                            "outside\noutside\"inside\\ninside\"outside\noutside"
            ]
        , describe "convertMultiLineStringsInner"
            [ test "stops recursing on EOF" <|
                \_ ->
                    { edited = "", remainder = "", inside = False }
                        |> ElmAst.MultiLineString.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "", remainder = "", inside = False }
            , test "handles a single char" <|
                \_ ->
                    { edited = "", remainder = "x", inside = False }
                        |> ElmAst.MultiLineString.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "x", remainder = "", inside = False }
            , test "handles an opening multiline string token" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = False }
                        |> ElmAst.MultiLineString.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = True }
            , test "handles a closing multiline string token" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = True }
                        |> ElmAst.MultiLineString.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = False }
            , test "replaces multiline string with single line string" <|
                \_ ->
                    { edited = ""
                    , remainder = "outside\noutside\"\"\"inside\ninside\"\"\"outside\noutside"
                    , inside = False
                    }
                        |> ElmAst.MultiLineString.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "outside\noutside\"inside\\ninside\"outside\noutside"
                            , remainder = ""
                            , inside = False
                            }
            ]
        , describe "chompNextToken"
            [ test "converts newlines with escaped newlines when inside" <|
                \_ ->
                    { edited = "", remainder = "\n", inside = True }
                        |> ElmAst.MultiLineString.chompNextToken (ElmAst.MultiLineString.Other '\n')
                        |> Expect.equal
                            { edited = "\\n", remainder = "", inside = True }
            , test "does not convert newline when outside" <|
                \_ ->
                    { edited = "", remainder = "\n", inside = False }
                        |> ElmAst.MultiLineString.chompNextToken (ElmAst.MultiLineString.Other '\n')
                        |> Expect.equal
                            { edited = "\n", remainder = "", inside = False }
            , test "replaces multiline comment token with single double quote" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = False }
                        |> ElmAst.MultiLineString.chompNextToken (ElmAst.MultiLineString.MultiLineComment)
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = False }
            ]
        , describe "getToken"
            [ test "parses a multiline string token" <|
                \_ ->
                    "\"\"\""
                        |> ElmAst.MultiLineString.getToken
                        |> Expect.equal ElmAst.MultiLineString.MultiLineComment
            , test "parses an EOF" <|
                \_ ->
                    ""
                        |> ElmAst.MultiLineString.getToken
                        |> Expect.equal ElmAst.MultiLineString.EOF
            , test "parses any other char" <|
                \_ ->
                    "x"
                        |> ElmAst.MultiLineString.getToken
                        |> Expect.equal (ElmAst.MultiLineString.Other 'x')
            , test "igores trailing text" <|
                \_ ->
                    "xyz"
                        |> ElmAst.MultiLineString.getToken
                        |> Expect.equal (ElmAst.MultiLineString.Other 'x')
            ]
        , describe "chompHelp"
            [ test "chomps a multiline string token" <|
                \_ ->
                    "\"\"\"testing 1 2 3"
                        |> ElmAst.MultiLineString.chompHelp ElmAst.MultiLineString.MultiLineComment
                        |> Expect.equal { tokenStr = "\"\"\"", remainder = "testing 1 2 3" }
            , test "chomps an EOF" <|
                \_ ->
                    ""
                        |> ElmAst.MultiLineString.chompHelp ElmAst.MultiLineString.EOF
                        |> Expect.equal { tokenStr = "", remainder = "" }
            , test "chomps an other char" <|
                \_ ->
                    "xyz"
                        |> ElmAst.MultiLineString.chompHelp (ElmAst.MultiLineString.Other 'x')
                        |> Expect.equal { tokenStr = "x", remainder = "yz" }
            ]
        ]
