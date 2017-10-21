port module FindViewFunctionsApp exposing (..)

import SubjectModuleInfo


{- needs to be here due to compiler bug apparently -}

import Json.Decode
import Dict


port return : List String -> Cmd msg


type alias Flags =
    { sourceCode : String }


init : Flags -> ( (), Cmd () )
init { sourceCode } =
    let
        viewFunctions : List String
        viewFunctions =
            sourceCode
                |> SubjectModuleInfo.getModuleInfo
                |> .viewFunctions
                |> Dict.keys
    in
        () ! [ return viewFunctions ]


main : Program Flags () ()
main =
    Platform.programWithFlags
        { init = init
        , update = (\() () -> ( (), Cmd.none ))
        , subscriptions = (\() -> Sub.none)
        }
