module Types exposing (..)

import Navigation exposing (Location)


type alias Model =
    { sourceDirectories : List File
    , page : Page

    -- top level "private" fields -- Don't pay too much attention to these yet! Pages should not need to know about them.
    , linkClicked : Bool
    , location : Location
    }


type Page
    = Loading
    | Home
    | DirectoryContents DirectoryContentsR
    | ElmModuleViewFunctions { elmModulePath : String, viewFunctions : List String }
    | ViewFunction (Result String String)


type alias DirectoryContentsR =
    { currentDirectory : String, files : List File }


type alias File =
    { fileType : FileType
    , name : String
    }


type FileType
    = ElmModule
    | Directory
