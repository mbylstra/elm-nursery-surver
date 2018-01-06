module ElmAst.Module exposing (..)

import String.Extra
import ElmAst.Type
    exposing
        ( TypeAliasDefinitionR
        , TypeAnnotation
        , UnionR
        , parseTypeAlias
        , parseTypeAnnotation
        , parseUnion
        )


-- import ImportStatement exposing (importStatement, parseImportStatement)
-- import ElmAst.ModuleStatement exposing (parseModuleStatement)

import ElmAst.Comments
import ElmAst.MultiLineString


-- import Parser

import ElmAst.ImportStatement as ImportStatement exposing (ImportStatement)


-- import Type
--     exposing
--         ( Block(Import, IgnoreBlock, TypeAliasDefinition, Union, TypeAnnotation, Module)
--         , ImportStatement
--         , ModuleStatement
--         , TypeAliasDefinitionR
--         , UnionR
--         , TypeAnnotation
--         )

import Maybe.Extra exposing (isJust)


type alias Ast =
    { dottedModulePath : String
    , importStatements : List ImportStatement
    , typeAliaseDefinitions : List TypeAliasDefinitionR
    , typeDefinitions : List UnionR
    , typeAnnotations : List TypeAnnotation
    }


type RawBlocks
    = List (List String)



-- type Block
--     = Module ModuleStatement
--     | TypeAliasDefinition TypeAliasDefinitionR
--     | Union UnionR
--     | Import ImportStatement
--     | TypeAnnotation TypeAnnotation
--     | IgnoreBlock
-- type alias ModuleInfo =
--     { dottedModulePath : String
--     , viewFunctions : ViewFunctions
--     , externalNamesModuleInfo : ExternalNamesModuleInfo
--     }
-- parseModuleStub : Result String Ast
-- parseModuleStub =
--     Ok
--         { dottedModulePath = ""
--         , importStatements = []
--         , typeAliaseDefinitions = []
--         , typeDefinitions = []
--         , typeAnnotations = []
--         }
-- parseModule : String -> Result String Ast
-- parseModule source =
--     let
--         wip =
--             source
--                 |> preProcessSource
--     in
--         parseModuleStub


preProcessSource : String -> String
preProcessSource source =
    source
        |> ElmAst.MultiLineString.convertMultiLineStrings
        |> ElmAst.Comments.removeComments
        |> removeEmptyLines


removeEmptyLines : String -> String
removeEmptyLines source =
    source
        |> String.lines
        |> List.filter (\line -> String.trim line /= "")
        |> String.join "\n"



-- let
--     rawBlocks =
--         source
--             |> String.lines
--             |> List.map removeOneLineComment
--             |> splitIntoRawBlocks
-- in
--     List.head rawBlocks
--     |> Result.fromMaybe "Module seems completely empty!"
--     |> Result.map
--         (classifyRawBlock >>
--             (\taggedRawBlock ->
--                 case taggedRawBlock of
--                     ImportStatementBlock contents ->
--
--
--             )
--
--     if
--
--     |> classifyRawBlocks
--     |> List.map parseBlock
--     |> List.map Result.toMaybe
--     |> List.filterMap identity


{-| This makes the assumption that any line that starts with a character another
than a space must be starting a new "block" such as an import, type annotation, etc
-}
splitIntoRawBlocks : String -> List String
splitIntoRawBlocks sourceCode =
    let
        processedSource =
            preProcessSource sourceCode
    in
        case (String.lines processedSource) of
            [] ->
                []

            line :: [] ->
                [ line ]

            line :: restLines ->
                restLines
                    |> List.foldl
                        (\line { blocks, currBlock } ->
                            if (line |> String.startsWith " ") then
                                { blocks = blocks
                                , currBlock = currBlock ++ "\n" ++ line
                                }
                            else
                                { blocks = blocks ++ [ currBlock ]
                                , currBlock = line
                                }
                        )
                        { blocks = [], currBlock = line }
                    |> \{ blocks, currBlock } ->
                        blocks ++ [ currBlock ]


type ClassifiedRawBlock
    = ModuleStatementBlock String
    | ImportStatementBlock String
    | TypeAliasDefinitionBlock String
    | TypeDefinitionBlock String
    | TypeAnnotationBlock String
    | FunctionDefinitionBlock String
    | IgnoreBlock String


classifyRawBlock : String -> ClassifiedRawBlock
classifyRawBlock s =
    if
        (s |> String.startsWith "module")
            || (s |> String.startsWith "port module")
    then
        ModuleStatementBlock s
    else if s |> String.startsWith "port" then
        -- we don't care about port statements, but we need to
        -- put this high up in the chain so it doesn't get incorrectly
        -- classified as a TypeAnnotationBlock
        IgnoreBlock s
    else if s |> String.startsWith "import" then
        ImportStatementBlock (replaceNewLinesWithSpaces s)
    else if s |> String.startsWith "type alias" then
        TypeAliasDefinitionBlock s
    else if s |> String.startsWith "type" then
        TypeDefinitionBlock s
    else if s |> String.contains "=" then
        FunctionDefinitionBlock s
        -- the ordering here is really important. type annotations never contain ='s
        -- (as of Elm's current syntax), but function definitions contain both ='s and :'s
    else if s |> String.contains ":" then
        TypeAnnotationBlock s
    else
        IgnoreBlock s



-- getModuleStatementContents : List String -> Maybe String
-- getModuleStatementContents rawBlocks =
--     rawBlocks
--         |> List.foldl
--             (\rawBlock acc ->
--                 if isJust acc then
--                     acc
--                 else
--                     case classifyRawBlock rawBlock of
--                         ModuleStatementBlock contents ->
--                             Just contents
--
--                         _ ->
--                             Nothing
--             )
--             Nothing
-- classifyRawBlocks : List String -> List RawBlock
-- classifyRawBlocks strings =
--     strings
--         |> List.map classifyRawBlock
-- parseBlock : RawBlock -> Result Parser.Error Block
-- parseBlock rawBlock =
--     case rawBlock of
--         EmptyLines string ->
--             Ok <| IgnoreBlock
--
--         ImportStatementBlock string ->
--             parseImportStatement string
--                 |> Result.map Import
--
--         TypeAliasDefinitionBlock string ->
--             parseTypeAlias string
--                 |> Result.map TypeAliasDefinition
--
--         TypeDefinition string ->
--             parseUnion string
--                 |> Result.map Union
--
--         TypeAnnotationBlock string ->
--             parseTypeAnnotation string
--                 |> Result.map TypeAnnotation
--
--         ModuleStatementBlock string ->
--             parseModuleStatement string
--                 |> Result.map Module
--
--         FunctionDefinitionBlock string ->
--             Ok IgnoreBlock
--
--         UnknownBlock string ->
--             Ok IgnoreBlock
--
--


replaceNewLinesWithSpaces : String -> String
replaceNewLinesWithSpaces s =
    s |> String.Extra.replace "\n" " "
