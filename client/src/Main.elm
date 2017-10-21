module Main exposing (..)

import Html exposing (Html, text, div, img, h1, li, a, ul, span, p, h2, h3, pre)
import Html.Attributes exposing (src, href, class)
import Http
import Lib exposing (arrayAsTuple2)
import Icons
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
