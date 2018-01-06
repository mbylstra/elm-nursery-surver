module ElmAst.ModuleTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import ElmAst.Module


-- import ElmAst.Module
--     exposing
--         ( parseModule
--         , removeOneLineComments
--         , splitIntoBlocks
--         , classifyBlocks
--         )


suite : Test
suite =
    describe "ElmAst.Module.elm"
        -- [ describe "parseModule"
        --     [ test "parses the module statement if there is one" <|
        --         \_ ->
        --             joinNl
        --                 [ "module Module1 exposing (..)"
        --                 ]
        --                 |> ElmAst.Module.parseModule
        --                 |> Expect.equal (Ok { emptyAst | dottedModulePath = "Module1" })
        --     , test " if  is no module statement, don't fail, but assume module Main exposing (..)" <|
        --         \_ ->
        --             joinNl
        --                 [ "x = 2"
        --                 ]
        --                 |> ElmAst.Module.parseModule
        --                 |> Expect.equal (Ok { emptyAst | dottedModulePath = "Main" })
        --     , test "parses an import statement if there is one" <|
        --         \_ ->
        --             joinNl
        --                 [ "module Module1 exposing (..)"
        --                 , "import Foo"
        --                 ]
        --                 |> ElmAst.Module.parseModule
        --                 |> Expect.equal (Ok { emptyAst | dottedModulePath = "Module1" })
        --     ]
        [ describe "preProcessSource" <|
            [ test "removes comments, converts multiline strings to single line strings, removes empty lines" <|
                \_ ->
                    joinNl
                        [ "x = 1 -- remove me"
                        , "{- remove me -}"
                        , ""
                        , "z = \"\"\""
                        , "hello"
                        , "\"\"\""
                        ]
                        |> ElmAst.Module.preProcessSource
                        |> Expect.equal "x = 1 \nz = \"\\nhello\\n\""
            ]
        , describe "removeEmptyLines" <|
            [ test "removes comments, converts multiline strings to single line strings, removes empty lines" <|
                \_ ->
                    joinNl
                        [ "a"
                        , ""
                        , " "
                        , "\t"
                        , "b"
                        ]
                        |> ElmAst.Module.removeEmptyLines
                        |> Expect.equal "a\nb"
            ]
        , describe "splitIntoBlocks"
            [ test "does it's thing" <|
                \_ ->
                    joinNl
                        [ "#"
                        , "#"
                        , " #"
                        , "#"
                        , " #"
                        , "  #"
                        , "#"
                        ]
                        |> ElmAst.Module.splitIntoRawBlocks
                        |> Expect.equal
                            [ "#"
                            , "#\n #"
                            , "#\n #\n  #"
                            , "#"
                            ]
            ]
        , describe "classifyRawBocks"
            [ test "classifies module statements" <|
                \_ ->
                    "module Main exposing (..)"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.ModuleStatementBlock "module Main exposing (..)")
            , test "classifies port module statements" <|
                \_ ->
                    "port module Main exposing (..)"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.ModuleStatementBlock "port module Main exposing (..)")
            , test "classifies import statements" <|
                \_ ->
                    "import Foo"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.ImportStatementBlock "import Foo")
            , test "classifies type alias definitions" <|
                \_ ->
                    "type alias Foo = Bar"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.TypeAliasDefinitionBlock "type alias Foo = Bar")
            , test "classifies type definitions" <|
                \_ ->
                    "type Id = Id String"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.TypeDefinitionBlock "type Id = Id String")
            , test "classifies type annotations" <|
                \_ ->
                    "view : Model -> Html msg"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.TypeAnnotationBlock "view : Model -> Html msg")
            , test "classifies function definitions" <|
                \_ ->
                    "x = 1"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.FunctionDefinitionBlock "x = 1")
            , test "classifies a port definition block as something we want to ignore" <|
                \_ ->
                    "port check : String -> Cmd msg"
                        |> ElmAst.Module.classifyRawBlock
                        |> Expect.equal (ElmAst.Module.IgnoreBlock "port check : String -> Cmd msg")
            , test "doesn't get tripped up by a function with a : in it" <|
                \_ ->
                    let
                        block =
                            joinNl
                                [ "x = case x of"
                                , "    [] :: xs -> xs"
                                ]
                    in
                        block
                            |> ElmAst.Module.classifyRawBlock
                            |> Expect.equal (ElmAst.Module.FunctionDefinitionBlock block)
            ]

        -- , describe "splitIntoRawBlocks"
        --     [ test "it does something" <|
        --         _ ->
        --             let
        --                 code =
        --                   [ "module Module1 exposing (..)"
        --                   , type
        --             in
        --                 ElmAst.Module.splitIntoRawBlocks code
        --                     |> Expect.equal something
        --     ]
        -- describe "ElmAst.Module.elm"
        --     [ describe "parseModule"
        --         [ test "basic test" <|
        --             \_ ->
        --                 ([ "module Main exposing (..)"
        --                  , "type Result error value = Ok value | Err error"
        --                  ]
        --                     |> String.join "\n"
        --                 )
        --                     |> String.lines
        --                     |> removeOneLineComments
        --                     |> splitIntoBlocks
        --                     |> classifyBlocks
        --                     |> Expect.equal
        --                         [ ElmAst.Module.ModuleStatementBlock "module Main exposing (..)"
        --                         , ElmAst.Module.TypeDefinition "type Result error value = Ok value | Err error"
        --                         ]
        --         , test "test with multi line comments" <|
        --             \_ ->
        --                 ([ "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
        --                  , "`Err` meaning that there was some failure."
        --                  , "-}"
        --                  , "type Result error value = Ok value | Err error"
        --                  ]
        --                     |> String.join "\n"
        --                 )
        --                     |> String.lines
        --                     |> removeOneLineComments
        --                     |> splitIntoBlocks
        --                     |> classifyBlocks
        --                     |> Expect.equal
        --                         [ ElmAst.Module.EmptyLines "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
        --                         , ElmAst.Module.EmptyLines "`Err` meaning that there was some failure."
        --                         , ElmAst.Module.EmptyLines "-}"
        --                         , ElmAst.Module.TypeDefinition "type Result error value = Ok value | Err error"
        --                         ]
        --         ]
        -- This is basically too annoying to test - we just want to know that right functions are
        -- called. The functions themselves are already tested.
        -- , describe "parseBlock"
        --
        --     [ test "EmptyLines" <|
        --         \_ ->
        --             ElmAst.Module.parseBlock (ElmAst.Module.EmptyLines "\n\n")
        --                 |> Expect.equal (Ok Types.IgnoreBlock)
        --     , test "ImportStatementBlock" <|
        --         \_ ->
        --             ElmAst.Module.parseBlock (ElmAst.Module.ImportStatementBlock "\n\n")
        --                 |> Expect.equal (Ok Types.Import)
        --     ]
        -- , describe "replaceNewLinesWithSpaces"
        --     [ test "that they are replaced with spaces :)" <|
        --         \_ ->
        --             ElmAst.Module.replaceNewLinesWithSpaces
        --                 (ElmAst.Module.replaceNewLinesWithSpaces "one\ntwo\n")
        --                 |> Expect.equal "one two "
        --     ]
        ]



-- emptyAst : ElmAst.Module.Ast
-- emptyAst =
--     { dottedModulePath = "Module1"
--     , importStatements = []
--     , typeAliaseDefinitions = []
--     , typeDefinitions = []
--     , typeAnnotations = []
--     }


joinNl : List String -> String
joinNl lines =
    String.join "\n" lines
