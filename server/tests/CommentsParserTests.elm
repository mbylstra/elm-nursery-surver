module CommentsParserTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import CommentsParser


suite : Test
suite =
    describe "CommentsParser.elm"
        [ describe "removeMultiLineCommentsInner"
            [ test "stops recursing on EOF" <|
                \_ ->
                    { edited = "", remainder = "", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 0 }
            , test "handles a single char" <|
                \_ ->
                    { edited = "", remainder = "x", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "x", remainder = "", level = 0 }
            , test "handles an opening comment" <|
                \_ ->
                    { edited = "", remainder = "{-", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 1 }
            , test "handles a closing comment" <|
                \_ ->
                    { edited = "", remainder = "-}", level = 1 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 0 }
            , test "removes a regular comment" <|
                \_ ->
                    { edited = "", remainder = "a{-b-}c", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            , test "removes a doc comment" <|
                \_ ->
                    { edited = "", remainder = "a{-|b-}c", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            , test "removes a nested comment" <|
                \_ ->
                    { edited = "", remainder = "a{-{-b-}-}c", level = 0 }
                        |> CommentsParser.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            ]
        , describe "getToken"
            [ test "parses a CommentOpener" <|
                \_ ->
                    "{-"
                        |> CommentsParser.getToken
                        |> Expect.equal CommentsParser.CommentOpener
            , test "parses a CommentCloser" <|
                \_ ->
                    "-}"
                        |> CommentsParser.getToken
                        |> Expect.equal CommentsParser.CommentCloser
            , test "parses an EOF" <|
                \_ ->
                    ""
                        |> CommentsParser.getToken
                        |> Expect.equal CommentsParser.EOF
            , test "parses any other char" <|
                \_ ->
                    "x"
                        |> CommentsParser.getToken
                        |> Expect.equal (CommentsParser.Other 'x')
            , test "igores trailing text" <|
                \_ ->
                    "xyz"
                        |> CommentsParser.getToken
                        |> Expect.equal (CommentsParser.Other 'x')
            ]
        , describe "chompNextToken"
            [ test "chomps a single char when at level 0" <|
                \_ ->
                    { edited = "", remainder = "xyz", level = 0 }
                        |> CommentsParser.chompNextToken (CommentsParser.Other 'x')
                        |> Expect.equal
                            { edited = "x", remainder = "yz", level = 0 }
            , test "does not include single char when at level 1" <|
                \_ ->
                    { edited = "", remainder = "xyz", level = 1 }
                        |> CommentsParser.chompNextToken (CommentsParser.Other 'x')
                        |> Expect.equal
                            { edited = "", remainder = "yz", level = 1 }
            ]
        , describe "chompHelp"
            [ test "chomps a CommentOpener" <|
                \_ ->
                    "{- testing 1 2 3"
                        |> CommentsParser.chompHelp CommentsParser.CommentOpener
                        |> Expect.equal { tokenStr = "{-", remainder = " testing 1 2 3" }
            , test "chomps a CommentCloser" <|
                \_ ->
                    "-} testing 1 2 3"
                        |> CommentsParser.chompHelp CommentsParser.CommentCloser
                        |> Expect.equal { tokenStr = "-}", remainder = " testing 1 2 3" }
            , test "chomps an EOF" <|
                \_ ->
                    ""
                        |> CommentsParser.chompHelp CommentsParser.EOF
                        |> Expect.equal { tokenStr = "", remainder = "" }
            , test "chomps an other char" <|
                \_ ->
                    "xyz"
                        |> CommentsParser.chompHelp (CommentsParser.Other 'x')
                        |> Expect.equal { tokenStr = "x", remainder = "yz" }
            ]
        ]
