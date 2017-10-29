module DataGeneration exposing (..)

import Dict exposing (Dict)
import Helpers exposing (unsafeDictGet, unsafeListHead)
import Types exposing (..)
import ToQualified exposing (qualifyAllTypes)
import String.Extra
import Combinations


type alias InstantiatedTypeVars =
    List QualifiedType


generateViewFunctions : AllTypes -> List String
generateViewFunctions unqualifiedAllTypes =
    let
        ({ subjectModuleInfo, allModulesInfo } as allTypes) =
            qualifyAllTypes unqualifiedAllTypes
    in
        subjectModuleInfo.viewFunctions
            |> Dict.toList
            |> List.map (generateViewFunction allTypes subjectModuleInfo.dottedModulePath)


generateViewFunction : QualifiedAllTypes -> DottedModulePath -> ( String, QualifiedType ) -> String
generateViewFunction allTypes dottedModulePath ( functionName, functionType ) =
    let
        imports : String
        imports =
            Dict.keys allTypes.allModulesInfo
                |> List.map (\dottedModulePath -> "import " ++ dottedModulePath)
                |> (::) "import Html"
                |> String.join "\n"
    in
        case functionType of
            QualifiedLambda leftType rightType ->
                let
                    argTypes : List QualifiedType
                    argTypes =
                        lambdaToList leftType rightType
                            -- remove the last arg (it's just the Html msg one at the end)
                            |> List.reverse
                            |> List.drop 1
                            |> List.reverse

                    args : List (List String)
                    args =
                        argTypes
                            |> List.map (generateData allTypes [])

                    argsCombinations : List (List String)
                    argsCombinations =
                        args
                            |> Combinations.combinations

                    argListToString : List String -> String
                    argListToString argList =
                        argList |> String.join " "

                    qualifiedFunctionName =
                        dottedModulePath ++ "." ++ functionName

                    views : List String
                    views =
                        argsCombinations
                            |> List.map
                                (\argList ->
                                    (qualifiedFunctionName
                                        ++ " "
                                        ++ argListToString argList
                                    )
                                        |> String.Extra.replace ", " ",\n"
                                )

                    code : String
                    code =
                        (imports
                            ++ ((Debug.log "views" views) |> String.join "\n")
                        )
                            -- something extremly basic prettification, that will then get cleaned up
                            -- by elm-format, so it doesn't all end up on one line
                            |> wrapInHtmlProgram
                in
                    code

            _ ->
                let
                    code : String
                    code =
                        imports
                            ++ "\nstaticView = "
                            |> String.Extra.replace ", " ",\n"
                            |> wrapInHtmlProgram
                in
                    code


lambdaToList : QualifiedType -> QualifiedType -> List QualifiedType
lambdaToList leftType rightType =
    leftType
        :: (case rightType of
                QualifiedLambda nextLeftType nextRightType ->
                    (lambdaToList nextLeftType nextRightType)

                _ ->
                    [ rightType ]
           )


{-| generate a bunch of potential instantiations for a QualifiedType
-}
generateData : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedType -> List String
generateData ({ subjectModuleInfo, allModulesInfo } as allTypes) instantiatedTypeVars tipe =
    case tipe of
        QualifiedVar varName ->
            case List.head instantiatedTypeVars of
                Just instantiatedType ->
                    case instantiatedType of
                        QualifiedVar _ ->
                            [ "\"String was chosen for wildcard type\"" ]

                        _ ->
                            generateData allTypes instantiatedTypeVars (Debug.log "instantiatedType" instantiatedType)

                Nothing ->
                    [ "\"String was chosen for wildcard type\"" ]

        -- QualifiedLambda leftType rightType ->
        --     generateLambda allTypes instantiatedTypeVars leftType rightType
        -- QualifiedTuple tipes ->
        --     "("
        --         ++ (tipes |> List.map (generateData allTypes instantiatedTypeVars) |> String.join ", ")
        -- ++ ")"
        QualifiedType ({ dottedModulePath, name } as qualifiedName) typeArguments ->
            case name of
                "Int" ->
                    [ "0", "1", "2" ]

                "String" ->
                    [ "\"hello\""
                    , "\"A medium length string\""
                    , "\"A very very very, I mean really, really, not joking, looonnnnngggggg string\""
                    ]

                "Bool" ->
                    [ "True", "False" ]

                "Float" ->
                    [ "1.0", "0.0", "-1.0" ]

                "Html" ->
                    [ """(Html.text "hello")""" ]

                "List" ->
                    let
                        listType =
                            unsafeListHead typeArguments

                        singleOptions =
                            generateData allTypes instantiatedTypeVars listType
                    in
                        singleOptions |> List.map (\option -> "[" ++ option ++ "]")

                "Set" ->
                    let
                        listType =
                            unsafeListHead typeArguments

                        singleOptions =
                            generateData allTypes instantiatedTypeVars listType
                    in
                        singleOptions |> List.map (\option -> "Set.fromList [" ++ option ++ "]")

                "Date" ->
                    [ "Date.fromTime 1506233184" ]

                -- This is just temporary for a proof of concept
                "DatePicker" ->
                    [ "DatePicker.initFromDate (Date.fromTime 1506233184)" ]

                _ ->
                    let
                        -- typeArguments
                        -- |> List.map qualifyTypeArgument
                        _ =
                            1
                    in
                        substituteType allTypes qualifiedName typeArguments

        -- QualifiedRecord fields _ ->
        --     let
        --         generateFieldData ( name, tipe ) =
        --             name ++ " = " ++ (generateData allTypes instantiatedTypeVars tipe)
        --     in
        --         "{"
        --             ++ (fields
        --                     |> List.map generateFieldData
        --                     |> String.join ", "
        --                )
        --             ++ "}"
        _ ->
            Debug.crash "This is type that's in the too hard basket for now"



-- generateLambda : QualifiedAllTypes -> InstantiatedTypeVars -> QualifiedType -> QualifiedType -> List String
-- generateLambda allTypes instantiatedTypeVars leftType rightType =
--     lambdaToList leftType rightType
--         |> List.reverse
--         |> (\argsList ->
--                 case argsList of
--                     [] ->
--                         Debug.crash "this shouldn't be possible"
--
--                     returnValue :: [] ->
--                         Debug.crash "this shouldn't be possible"
--
--                     returnValue :: arg1 :: args ->
--                         let
--                             numIgnoredArgs =
--                                 1 + List.length args
--
--                             returnedValue =
--                                 generateData allTypes instantiatedTypeVars returnValue
--                         in
--                             "(\\" ++ (List.repeat numIgnoredArgs "_" |> String.join " ") ++ " -> " ++ returnedValue ++ ")"
--            )


generateFromUnionType : QualifiedAllTypes -> DottedModulePath -> InstantiatedTypeVars -> QualifiedUnionR -> List String
generateFromUnionType allTypes dottedModulePath instantiatedTypeVars { name, typeVars, definition } =
    let
        firstConstructor =
            definition
                |> unsafeListHead
    in
        firstConstructor |> generateFromTypeConstructor allTypes dottedModulePath instantiatedTypeVars


generateFromTypeConstructor : QualifiedAllTypes -> DottedModulePath -> InstantiatedTypeVars -> QualifiedTypeConstructor -> List String
generateFromTypeConstructor allTypes dottedModulePath instantiateTypeVars ( name, args ) =
    let
        generateArg : QualifiedType -> List String
        generateArg tipe =
            generateData allTypes instantiateTypeVars tipe
                |> List.map (\code -> " ( " ++ code ++ " ) ")

        -- for each arg we get a list of args
        argsCombinations : List (List String)
        argsCombinations =
            args
                |> List.map generateArg
                |> Combinations.combinations

        argListToString : List String -> String
        argListToString argList =
            argList |> String.join " "
    in
        argsCombinations
            |> List.map
                (\argList ->
                    dottedModulePath ++ "." ++ name ++ " " ++ (argListToString argList)
                )



-- We need the full type, not just the type name


coreDifficultTypes : List QualifiedName
coreDifficultTypes =
    [ { dottedModulePath = "Dict", name = "Dict" }
    , { dottedModulePath = "Set", name = "Set" }
    ]


substituteType : QualifiedAllTypes -> QualifiedName -> InstantiatedTypeVars -> List String
substituteType ({ allModulesInfo } as allTypes) ({ dottedModulePath, name } as qualifiedName) instantiatedTypeVars =
    if List.member qualifiedName coreDifficultTypes then
        [ "(TOO HARD BASKET)" ]
    else
        let
            subjectModuleInfo =
                allModulesInfo |> unsafeDictGet "DataGeneration.elm 129" dottedModulePath
        in
            case Dict.get name subjectModuleInfo.typeAliases of
                Just tipe ->
                    generateData allTypes instantiatedTypeVars tipe

                Nothing ->
                    case Dict.get name subjectModuleInfo.unionTypes of
                        Just unionDefinition ->
                            generateFromUnionType allTypes dottedModulePath instantiatedTypeVars unionDefinition

                        Nothing ->
                            Debug.crash ("could not find " ++ toString qualifiedName)


wrapInHtmlProgram : String -> String
wrapInHtmlProgram code =
    code ++ """
    |> Html.map (\\_ -> ())

main =
    Html.beginnerProgram
        { model = ()
        , view = (\\() -> staticView)
        , update = (\\() () -> ())
        }
        """
