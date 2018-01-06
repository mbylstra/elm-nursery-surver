module ElmAst.CommentsTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import ElmAst.Comments


suite : Test
suite =
    describe "ElmAst.Comments.elm"
        [ describe "removeComments"
            [ test "the comment trick works - not commented out" <|
                \_ ->
                    "{--} add x y = x + y --}"
                        |> ElmAst.Comments.removeComments
                        |> Expect.equal " add x y = x + y "
            , test "the comment trick works - commented out" <|
                \_ ->
                    "{-- add x y = x + y --}"
                        |> ElmAst.Comments.removeComments
                        |> Expect.equal ""
            ]
        , describe "removeMultiLineComments"
            [ test "works on a simple case" <|
                \_ ->
                    "foo {- bar -} baz"
                        |> ElmAst.Comments.removeMultiLineComments
                        |> Expect.equal
                            "foo  baz"
            ]
        , describe "removeMultiLineCommentsInner"
            [ test "stops recursing on EOF" <|
                \_ ->
                    { edited = "", remainder = "", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 0 }
            , test "handles a single char" <|
                \_ ->
                    { edited = "", remainder = "x", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "x", remainder = "", level = 0 }
            , test "handles an opening comment" <|
                \_ ->
                    { edited = "", remainder = "{-", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 1 }
            , test "handles a closing comment" <|
                \_ ->
                    { edited = "", remainder = "-}", level = 1 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 0 }
            , test "removes a regular comment" <|
                \_ ->
                    { edited = "", remainder = "a{-b-}c", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            , test "removes a doc comment" <|
                \_ ->
                    { edited = "", remainder = "a{-|b-}c", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            , test "removes a nested comment" <|
                \_ ->
                    { edited = "", remainder = "a{-{-b-}-}c", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "ac", remainder = "", level = 0 }
            , test "does not get tripped up by records" <|
                \_ ->
                    { edited = "", remainder = "type alias Foo = {a:Int,b:Int}", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "type alias Foo = {a:Int,b:Int}", remainder = "", level = 0 }
            , test "handles extra dashes" <|
                \_ ->
                    { edited = "", remainder = "{--}", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "", remainder = "", level = 0 }
            , test "level is always positive" <|
                \_ ->
                    { edited = "", remainder = "-}", level = 0 }
                        |> ElmAst.Comments.removeMultiLineCommentsInner
                        |> Expect.equal
                            { edited = "-}", remainder = "", level = 0 }
            ]
        , describe "getToken"
            [ test "parses a CommentOpener" <|
                \_ ->
                    "{-"
                        |> ElmAst.Comments.getToken
                        |> Expect.equal ElmAst.Comments.CommentOpener
            , test "parses a CommentCloser" <|
                \_ ->
                    "-}"
                        |> ElmAst.Comments.getToken
                        |> Expect.equal ElmAst.Comments.CommentCloser
            , test "parses an EOF" <|
                \_ ->
                    ""
                        |> ElmAst.Comments.getToken
                        |> Expect.equal ElmAst.Comments.EOF
            , test "parses any other char" <|
                \_ ->
                    "x"
                        |> ElmAst.Comments.getToken
                        |> Expect.equal (ElmAst.Comments.Other 'x')
            , test "igores trailing text" <|
                \_ ->
                    "xyz"
                        |> ElmAst.Comments.getToken
                        |> Expect.equal (ElmAst.Comments.Other 'x')
            ]
        , describe "chompNextToken"
            [ test "chomps a single char when at level 0" <|
                \_ ->
                    { edited = "", remainder = "xyz", level = 0 }
                        |> ElmAst.Comments.chompNextToken (ElmAst.Comments.Other 'x')
                        |> Expect.equal
                            { edited = "x", remainder = "yz", level = 0 }
            , test "does not include single char when at level 1" <|
                \_ ->
                    { edited = "", remainder = "xyz", level = 1 }
                        |> ElmAst.Comments.chompNextToken (ElmAst.Comments.Other 'x')
                        |> Expect.equal
                            { edited = "", remainder = "yz", level = 1 }
            ]
        , describe "chompHelp"
            [ test "chomps a CommentOpener" <|
                \_ ->
                    "{- testing 1 2 3"
                        |> ElmAst.Comments.chompHelp ElmAst.Comments.CommentOpener
                        |> Expect.equal { tokenStr = "{-", remainder = " testing 1 2 3" }
            , test "chomps a CommentCloser" <|
                \_ ->
                    "-} testing 1 2 3"
                        |> ElmAst.Comments.chompHelp ElmAst.Comments.CommentCloser
                        |> Expect.equal { tokenStr = "-}", remainder = " testing 1 2 3" }
            , test "chomps an EOF" <|
                \_ ->
                    ""
                        |> ElmAst.Comments.chompHelp ElmAst.Comments.EOF
                        |> Expect.equal { tokenStr = "", remainder = "" }
            , test "chomps an other char" <|
                \_ ->
                    "xyz"
                        |> ElmAst.Comments.chompHelp (ElmAst.Comments.Other 'x')
                        |> Expect.equal { tokenStr = "x", remainder = "yz" }
            ]
        , describe "removeOneLineComments" <|
            [ test "removes a single trailing one line comment from one line" <|
                \_ ->
                    "abc --testing 1 2 3"
                        |> ElmAst.Comments.removeOneLineComments
                        |> Expect.equal "abc "
            , test "removes comments from multiple lines" <|
                \_ ->
                    "abc --testing 1 2 3\ndef --testing 4 5 6"
                        |> ElmAst.Comments.removeOneLineComments
                        |> Expect.equal "abc \ndef "
            , test "removes multiple comments on the one line" <|
                \_ ->
                    "abc --testing 1 2 3 --testing 1 2 3"
                        |> ElmAst.Comments.removeOneLineComments
                        |> Expect.equal "abc "
            ]
        ]
