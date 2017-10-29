module DataGenerationTest exposing (..)

import DataGeneration
    exposing
        ( lambdaToList
        , generateLambda
        , generateFromTypeConstructor
        , generateFromUnionType
        )
import Expect exposing (Expectation, equalSets)
import Test exposing (..)
import Types exposing (..)
import Dict


int : QualifiedType
int =
    QualifiedType { dottedModulePath = "__CORE__", name = "Int" } []


emptyAllTypes : QualifiedAllTypes
emptyAllTypes =
    { subjectModuleInfo = emptySubjectModuleInfo
    , allModulesInfo = Dict.empty
    }


emptySubjectModuleInfo : QualifiedModuleInfo
emptySubjectModuleInfo =
    { viewFunctions = Dict.empty
    , dottedModulePath = ""
    , unionTypes = Dict.empty
    , typeAliases = Dict.empty
    }


suite : Test
suite =
    describe "DataGeneration.elm"
        [ describe "lambdaToList"
            [ test "basic case" <|
                \_ ->
                    lambdaToList int int
                        |> Expect.equal
                            [ int, int ]
            , test "multiple lambdas" <|
                \_ ->
                    lambdaToList int (QualifiedLambda int int)
                        |> Expect.equal
                            [ int, int, int ]
            ]
        , describe "generateLambda"
            [ test "basic case" <|
                \_ ->
                    (generateLambda
                        emptyAllTypes
                        []
                        int
                        int
                    )
                        |> Expect.equal
                            [ "(\\_ -> 0)"
                            , "(\\_ -> 1)"
                            , "(\\_ -> 2)"
                            ]
            , test "more than one arg" <|
                \_ ->
                    (generateLambda
                        emptyAllTypes
                        []
                        int
                        (QualifiedLambda int int)
                    )
                        |> Expect.equal
                            [ "(\\_ _ -> 0)"
                            , "(\\_ _ -> 1)"
                            , "(\\_ _ -> 2)"
                            ]
            ]
        , describe "generateFromTypeConstructor"
            [ test "simple type constructor with no args" <|
                \_ ->
                    (generateFromTypeConstructor
                        emptyAllTypes
                        "DummyModule"
                        []
                        ( "SomeTypeConstructor"
                        , []
                        )
                    )
                        |> Expect.equal
                            [ "DummyModule.SomeTypeConstructor" ]
            ]
        , describe "generateFromUnionType"
            [ test "simple type constructor with no args" <|
                \_ ->
                    (generateFromUnionType
                        emptyAllTypes
                        "DummyModule"
                        []
                        { name = "SomeUnionType"
                        , typeVars = []
                        , definition =
                            [ ( "ConstructorA"
                              , []
                              )
                            , ( "ConstructorB"
                              , []
                              )
                            ]
                        }
                    )
                        |> Expect.equal
                            [ "DummyModule.ConstructorA"
                            , "DummyModule.ConstructorB"
                            ]
            ]
        ]



-- generateFromUnionType :
--     QualifiedAllTypes
--     -> DottedModulePath
--     -> InstantiatedTypeVars
--     -> QualifiedUnionR
--     -> List String
-- type alias QualifiedUnionR =
--     { name : String
--     , typeVars : List String
--     , definition : QualifiedUnionDefinition
--     }
-- generateFromTypeConstructor : QualifiedAllTypes -> DottedModulePath -> InstantiatedTypeVars -> QualifiedTypeConstructor -> List String
-- type alias QualifiedTypeConstructor =
--     ( String, QualifiedTypeConstructorArgs )
-- Helpers ---------------------------------------------------------------------
