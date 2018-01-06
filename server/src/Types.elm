module Types exposing (..)

import Dict exposing (Dict)


-- type Block
--     = Module ModuleStatement
--     | TypeAliasDefinition TypeAliasDefinitionR
--     | Union UnionR
--     | Import ImportStatement
--     | TypeAnnotation TypeAnnotation
--     | IgnoreBlock


{-| it's just the dotted module name for now
-}
type alias ModuleStatement =
    String


type alias QualifiedName =
    { dottedModulePath : String, name : String }


type QualifiedType
    = QualifiedVar String
    | QualifiedLambda QualifiedType QualifiedType
    | QualifiedTuple (List QualifiedType)
    | QualifiedType QualifiedName (List QualifiedType)
    | QualifiedRecord (List ( String, QualifiedType )) (Maybe String)


type alias QualifiedTypeConstructor =
    ( String, QualifiedTypeConstructorArgs )


type alias QualifiedTypeConstructorArgs =
    List QualifiedType


type alias QualifiedUnionR =
    { name : String
    , typeVars : List String
    , definition : QualifiedUnionDefinition
    }


type alias QualifiedUnionDefinition =
    List QualifiedTypeConstructor


type alias FullModulePath =
    String



-- type alias ModuleInfo =
--     { dottedModulePath : String
--     , viewFunctions : ViewFunctions
--     , localTypeAliases : LocalTypeAliases
--     , localUnionTypes : LocalUnionTypes
--     , externalNamesModuleInfo : ExternalNamesModuleInfo
--     }


type alias QualifiedModuleInfo =
    { dottedModulePath : String
    , viewFunctions : Dict Name QualifiedType
    , typeAliases : Dict Name QualifiedType
    , unionTypes : Dict Name QualifiedUnionR
    }


type DefinitionLocation
    = Local
    | External String



-- type alias LocalUnionTypes =
--     Dict Name UnionR
-- type alias ViewFunctions =
--     Dict Name Type
-- type alias LocalTypeAliases =
--     Dict Name Type


type alias ExternalNamesModuleInfo =
    Dict RawDottedName { dottedModulePath : String, name : String }


type alias RawDottedName =
    String


type alias Name =
    String


type alias DottedModulePath =
    String


type alias SourceCode =
    String


type alias ModuleToSource =
    Dict DottedModulePath SourceCode



-- type alias ModuleToModuleInfo =
--     Dict DottedModulePath (Maybe ModuleInfo)
-- type alias AllTypes =
--     { subjectModuleInfo : ModuleInfo
--     , allModulesInfo : ModulesInfo
--     }


type alias QualifiedAllTypes =
    { subjectModuleInfo : QualifiedModuleInfo
    , allModulesInfo : Dict DottedModulePath QualifiedModuleInfo
    }



-- type alias ModulesInfo =
--     Dict DottedModulePath ModuleInfo
