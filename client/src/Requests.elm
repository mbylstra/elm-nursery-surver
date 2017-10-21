module Requests exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Types exposing (DirectoryContentsR, File, FileType(..))


apiBaseUrl : String
apiBaseUrl =
    "http://localhost:3000/"


elmSourceDirectoriesRequest : Http.Request (List File)
elmSourceDirectoriesRequest =
    Http.get
        (apiBaseUrl ++ "api/elm-source-directories/")
        (Decode.list (Decode.string |> Decode.map (\fileName -> File Directory fileName)))


directoryContentsRequest : String -> Http.Request DirectoryContentsR
directoryContentsRequest directory =
    let
        params =
            "?directory=" ++ Http.encodeUri directory
    in
        Http.get (apiBaseUrl ++ "api/directory-contents/" ++ params) directoryContentsDecoder


elmModuleViewFunctionsRequest : String -> Http.Request (List String)
elmModuleViewFunctionsRequest elmModulePath =
    let
        params =
            "?filePath=" ++ Http.encodeUri elmModulePath
    in
        Http.get (apiBaseUrl ++ "api/elm-module-view-functions/" ++ params) (Decode.list Decode.string)


generateViewFunctionRequest : { elmModulePath : String, viewFunction : String } -> Http.Request String
generateViewFunctionRequest { elmModulePath, viewFunction } =
    let
        params =
            "?elmModulePath=" ++ Http.encodeUri elmModulePath ++ "&viewFunction=" ++ viewFunction

        viewFunctionDecoder =
            Decode.string
    in
        Http.get (apiBaseUrl ++ "api/view-function/" ++ params) viewFunctionDecoder


fileDecoder : Decoder File
fileDecoder =
    Decode.map2
        (\fileName isDirectory ->
            let
                fileType =
                    case isDirectory of
                        True ->
                            Directory

                        False ->
                            ElmModule
            in
                File fileType fileName
        )
        (Decode.field "fileName" Decode.string)
        (Decode.field "isDirectory" Decode.bool)


directoryContentsDecoder : Decoder DirectoryContentsR
directoryContentsDecoder =
    Decode.map2 DirectoryContentsR
        (Decode.field "directory" Decode.string)
        (Decode.field "files" <| Decode.list fileDecoder)
