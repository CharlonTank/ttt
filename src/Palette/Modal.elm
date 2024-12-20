module Palette.Modal exposing (overlay, container)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Theme exposing (Theme)
import Palette.Animation as Animation

overlay : List (Attribute msg)
overlay =
    [ style "position" "fixed"
    , style "top" "0"
    , style "left" "0"
    , style "width" "100%"
    , style "height" "100%"
    , style "background-color" "rgba(0, 0, 0, 0.5)"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "z-index" "1000"
    ]

container : Theme -> List (Attribute msg)
container c =
    [ style "background-color" c.background
    , style "padding" "30px"
    , style "border-radius" "15px"
    , style "text-align" "center"
    ] ++ Animation.slideIn 