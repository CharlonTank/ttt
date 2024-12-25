module Palette.Button exposing (base, danger, disabled, primary, secondary, withShadow)

import Color
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Types exposing (UserConfig)


base : List (Attribute msg)
base =
    [ style "padding" "12px 20px"
    , style "font-size" "0.8em"
    , style "font-family" "inherit"
    , style "border" "none"
    , style "border-radius" "8px"
    , style "cursor" "pointer"
    , style "transition" "all 0.2s ease"
    ]


primary : UserConfig -> List (Attribute msg)
primary { c } =
    base
        ++ [ style "background-color" Color.primary
           , style "color" "white"
           ]


secondary : UserConfig -> List (Attribute msg)
secondary { c } =
    base
        ++ [ style "background-color" c.secondaryBackground
           , style "color" "white"
           ]


danger : UserConfig -> List (Attribute msg)
danger { c } =
    base
        ++ [ style "background-color" Color.danger
           , style "color" "white"
           ]


withShadow : List (Attribute msg)
withShadow =
    [ style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.2)" ]


disabled : List (Attribute msg)
disabled =
    [ style "opacity" "0.7"
    , style "cursor" "not-allowed"
    ]
