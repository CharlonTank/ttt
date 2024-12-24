module Palette.Animation exposing (fadeIn, fadeOut, pulse, slideIn)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

fadeIn : List (Attribute msg)
fadeIn =
    [ style "transition" "opacity 0.3s ease-in"
    , style "opacity" "1"
    ]

fadeOut : List (Attribute msg)
fadeOut =
    [ style "transition" "opacity 0.3s ease-out"
    , style "opacity" "0"
    ]

pulse : List (Attribute msg)
pulse =
    [ style "animation" "pulse 2s infinite" ]

slideIn : List (Attribute msg)
slideIn =
    [ style "animation" "slideIn 0.3s ease-out" ] 