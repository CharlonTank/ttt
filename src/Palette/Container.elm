module Palette.Container exposing (fullscreen, card)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Theme exposing (Theme)

fullscreen : List (Attribute msg)
fullscreen =
    [ style "min-height" "100vh"
    , style "width" "100%"
    , style "position" "absolute"
    , style "top" "0"
    , style "left" "0"
    ]

card : Theme -> List (Attribute msg)
card c =
    [ style "border-radius" "20px"
    , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
    , style "padding" "40px"
    , style "background-color" c.background
    ] 