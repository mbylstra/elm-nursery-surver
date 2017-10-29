module DummyViews exposing (..)

import Html exposing (Html, text)

type Size = Big | Small | Medium

type Color = Red | Blue | Green


view : Size -> Color -> Html msg
view size color =
    text "YOLO"
