module Lib exposing (..)

import Html.Events exposing (onWithOptions)
import Html.Attributes exposing (href)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)


link : msg -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link msg attributes content =
    Html.a
        ([ onWithOptions
            "click"
            { preventDefault = True, stopPropagation = False }
            (Decode.succeed msg)
         , href "#"
         ]
            ++ attributes
        )
        content


{-| <https://stackoverflow.com/a/42704577/343043>
-}
arrayAsTuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
arrayAsTuple2 a b =
    Decode.index 0 a
        |> Decode.andThen
            (\aVal ->
                Decode.index 1 b
                    |> Decode.andThen (\bVal -> Decode.succeed ( aVal, bVal ))
            )
