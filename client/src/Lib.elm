module Lib exposing (..)

import Html.Events exposing (onWithOptions)
import Html.Attributes exposing (href)
import Html exposing (Html, a)
import Json.Decode


link : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link msg attributes content =
    a
        ([ onWithOptions
            "click"
            { preventDefault = True, stopPropagation = False }
            (Json.Decode.succeed msg)
         , href "#"
         ]
            ++ attributes
        )
        content
