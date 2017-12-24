module MultiLineStringParserTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import MultiLineStringParser


suite : Test
suite =
    describe "MultiLineStringParser.elm"
        [ describe "removeMultiLineComments"
            [ test "works on a simple case" <|
                \_ ->
                    "outside\noutside\"\"\"inside\ninside\"\"\"outside\noutside"
                        |> MultiLineStringParser.convertMultiLineStrings
                        |> Expect.equal
                            "outside\noutside\"inside\\ninside\"outside\noutside"
            ]
        , describe "convertMultiLineStringsInner"
            [ test "stops recursing on EOF" <|
                \_ ->
                    { edited = "", remainder = "", inside = False }
                        |> MultiLineStringParser.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "", remainder = "", inside = False }
            , test "handles a single char" <|
                \_ ->
                    { edited = "", remainder = "x", inside = False }
                        |> MultiLineStringParser.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "x", remainder = "", inside = False }
            , test "handles an opening multiline string token" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = False }
                        |> MultiLineStringParser.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = True }
            , test "handles a closing multiline string token" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = True }
                        |> MultiLineStringParser.convertMultiLineStringsInner
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = False }
            , test "replaces multiline string with single line string" <|
                \_ ->
                    { edited = ""
                    , remainder = "outside\noutside\"\"\"inside\ninside\"\"\"outside\noutside"
                    , inside = False
                    }
                        |> MultiLineStringParser.convertMultiLineStringsInner
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
                        |> MultiLineStringParser.chompNextToken (MultiLineStringParser.Other '\n')
                        |> Expect.equal
                            { edited = "\\n", remainder = "", inside = True }
            , test "does not convert newline when outside" <|
                \_ ->
                    { edited = "", remainder = "\n", inside = False }
                        |> MultiLineStringParser.chompNextToken (MultiLineStringParser.Other '\n')
                        |> Expect.equal
                            { edited = "\n", remainder = "", inside = False }
            , test "replaces multiline comment token with single double quote" <|
                \_ ->
                    { edited = "", remainder = "\"\"\"", inside = False }
                        |> MultiLineStringParser.chompNextToken (MultiLineStringParser.MultiLineComment)
                        |> Expect.equal
                            { edited = "\"", remainder = "", inside = False }
            ]
        , describe "getToken"
            [ test "parses a multiline string token" <|
                \_ ->
                    "\"\"\""
                        |> MultiLineStringParser.getToken
                        |> Expect.equal MultiLineStringParser.MultiLineComment
            , test "parses an EOF" <|
                \_ ->
                    ""
                        |> MultiLineStringParser.getToken
                        |> Expect.equal MultiLineStringParser.EOF
            , test "parses any other char" <|
                \_ ->
                    "x"
                        |> MultiLineStringParser.getToken
                        |> Expect.equal (MultiLineStringParser.Other 'x')
            , test "igores trailing text" <|
                \_ ->
                    "xyz"
                        |> MultiLineStringParser.getToken
                        |> Expect.equal (MultiLineStringParser.Other 'x')
            ]
        , describe "chompHelp"
            [ test "chomps a multiline string token" <|
                \_ ->
                    "\"\"\"testing 1 2 3"
                        |> MultiLineStringParser.chompHelp MultiLineStringParser.MultiLineComment
                        |> Expect.equal { tokenStr = "\"\"\"", remainder = "testing 1 2 3" }
            , test "chomps an EOF" <|
                \_ ->
                    ""
                        |> MultiLineStringParser.chompHelp MultiLineStringParser.EOF
                        |> Expect.equal { tokenStr = "", remainder = "" }
            , test "chomps an other char" <|
                \_ ->
                    "xyz"
                        |> MultiLineStringParser.chompHelp (MultiLineStringParser.Other 'x')
                        |> Expect.equal { tokenStr = "x", remainder = "yz" }
            ]
        ]
