module ModuleParserTests exposing (..)

import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import ModuleParser


-- import ModuleParser
--     exposing
--         ( parseModule
--         , removeOneLineComments
--         , splitIntoBlocks
--         , classifyBlocks
--         )


suite : Test
suite =
    describe "ModuleParser.elm"
        -- [ describe "parseModule"
        --     [ test "parses the module statement if there is one" <|
        --         \_ ->
        --             joinNl
        --                 [ "module Module1 exposing (..)"
        --                 ]
        --                 |> ModuleParser.parseModule
        --                 |> Expect.equal (Ok { emptyAst | dottedModulePath = "Module1" })
        --     , test " if  is no module statement, don't fail, but assume module Main exposing (..)" <|
        --         \_ ->
        --             joinNl
        --                 [ "x = 2"
        --                 ]
        --                 |> ModuleParser.parseModule
        --                 |> Expect.equal (Ok { emptyAst | dottedModulePath = "Main" })
        --     , test "parses an import statement if there is one" <|
        --         \_ ->
        --             joinNl
        --                 [ "module Module1 exposing (..)"
        --                 , "import Foo"
        --                 ]
        --                 |> ModuleParser.parseModule
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
                        |> ModuleParser.preProcessSource
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
                        |> ModuleParser.removeEmptyLines
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
                        |> ModuleParser.splitIntoRawBlocks
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
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.ModuleStatementBlock "module Main exposing (..)")
            , test "classifies port module statements" <|
                \_ ->
                    "port module Main exposing (..)"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.ModuleStatementBlock "port module Main exposing (..)")
            , test "classifies import statements" <|
                \_ ->
                    "import Foo"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.ImportStatementBlock "import Foo")
            , test "classifies type alias definitions" <|
                \_ ->
                    "type alias Foo = Bar"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.TypeAliasDefinitionBlock "type alias Foo = Bar")
            , test "classifies type definitions" <|
                \_ ->
                    "type Id = Id String"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.TypeDefinitionBlock "type Id = Id String")
            , test "classifies type annotations" <|
                \_ ->
                    "view : Model -> Html msg"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.TypeAnnotationBlock "view : Model -> Html msg")
            , test "classifies function definitions" <|
                \_ ->
                    "x = 1"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.FunctionDefinitionBlock "x = 1")
            , test "classifies a port definition block as something we want to ignore" <|
                \_ ->
                    "port check : String -> Cmd msg"
                        |> ModuleParser.classifyRawBlock
                        |> Expect.equal (ModuleParser.IgnoreBlock "port check : String -> Cmd msg")
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
                            |> ModuleParser.classifyRawBlock
                            |> Expect.equal (ModuleParser.FunctionDefinitionBlock block)
            ]

        -- , describe "splitIntoRawBlocks"
        --     [ test "it does something" <|
        --         _ ->
        --             let
        --                 code =
        --                   [ "module Module1 exposing (..)"
        --                   , type
        --             in
        --                 ModuleParser.splitIntoRawBlocks code
        --                     |> Expect.equal something
        --     ]
        -- describe "ModuleParser.elm"
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
        --                         [ ModuleParser.ModuleStatementBlock "module Main exposing (..)"
        --                         , ModuleParser.TypeDefinition "type Result error value = Ok value | Err error"
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
        --                         [ ModuleParser.EmptyLines "{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an"
        --                         , ModuleParser.EmptyLines "`Err` meaning that there was some failure."
        --                         , ModuleParser.EmptyLines "-}"
        --                         , ModuleParser.TypeDefinition "type Result error value = Ok value | Err error"
        --                         ]
        --         ]
        -- This is basically too annoying to test - we just want to know that right functions are
        -- called. The functions themselves are already tested.
        -- , describe "parseBlock"
        --
        --     [ test "EmptyLines" <|
        --         \_ ->
        --             ModuleParser.parseBlock (ModuleParser.EmptyLines "\n\n")
        --                 |> Expect.equal (Ok Types.IgnoreBlock)
        --     , test "ImportStatementBlock" <|
        --         \_ ->
        --             ModuleParser.parseBlock (ModuleParser.ImportStatementBlock "\n\n")
        --                 |> Expect.equal (Ok Types.Import)
        --     ]
        -- , describe "replaceNewLinesWithSpaces"
        --     [ test "that they are replaced with spaces :)" <|
        --         \_ ->
        --             ModuleParser.replaceNewLinesWithSpaces
        --                 (ModuleParser.replaceNewLinesWithSpaces "one\ntwo\n")
        --                 |> Expect.equal "one two "
        --     ]
        ]



-- emptyAst : ModuleParser.Ast
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
