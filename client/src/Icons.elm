module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (attribute)


file : Svg msg
file =
    svg
        [ attribute "version" "1.1"
        , viewBox "0 0 12 16"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        , attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ]
        [ node "title"
            []
            [ text "file" ]
        , node "desc"
            []
            [ text "Created with Sketch." ]
        , defs []
            []
        , g [ fill "none", attribute "fill-rule" "evenodd", id "Octicons", attribute "stroke" "none", attribute "stroke-width" "1" ]
            [ g [ fill "#000000", id "file" ]
                [ Svg.path [ d "M6,5 L2,5 L2,4 L6,4 L6,5 L6,5 Z M2,8 L9,8 L9,7 L2,7 L2,8 L2,8 Z M2,10 L9,10 L9,9 L2,9 L2,10 L2,10 Z M2,12 L9,12 L9,11 L2,11 L2,12 L2,12 Z M12,4.5 L12,14 C12,14.55 11.55,15 11,15 L1,15 C0.45,15 0,14.55 0,14 L0,2 C0,1.45 0.45,1 1,1 L8.5,1 L12,4.5 L12,4.5 Z M11,5 L8,2 L1,2 L1,14 L11,14 L11,5 L11,5 Z", id "Shape" ]
                    []
                ]
            ]
        ]


directory : Svg msg
directory =
    svg
        [ attribute "version" "1.1"
        , viewBox "0 0 14 16"
        , attribute "xmlns" "http://www.w3.org/2000/svg"
        , attribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
        ]
        [ node "title"
            []
            [ text "file-directory" ]
        , node "desc"
            []
            [ text "Created with Sketch." ]
        , defs []
            []
        , g [ fill "none", attribute "fill-rule" "evenodd", id "Octicons", attribute "stroke" "none", attribute "stroke-width" "1" ]
            [ g [ fill "#000000", id "file-directory" ]
                [ Svg.path [ d "M13,4 L7,4 L7,3 C7,2.34 6.69,2 6,2 L1,2 C0.45,2 0,2.45 0,3 L0,13 C0,13.55 0.45,14 1,14 L13,14 C13.55,14 14,13.55 14,13 L14,5 C14,4.45 13.55,4 13,4 L13,4 Z M6,4 L1,4 L1,3 L6,3 L6,4 L6,4 Z", id "Shape" ]
                    []
                ]
            ]
        ]
