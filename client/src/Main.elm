module Main exposing (..)

import Types exposing (Model)
import State exposing (Msg(..), init, update)
import View exposing (view)
import Navigation


main : Program Never Model Msg
main =
    Navigation.program
        NewLocation
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
