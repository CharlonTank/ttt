module Palette.Utils exposing (fullWidth, gap15, margin10, maxWidth)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


fullWidth : List (Attribute msg)
fullWidth =
    [ style "width" "100%" ]


maxWidth : Int -> List (Attribute msg)
maxWidth px =
    [ style "max-width" (String.fromInt px ++ "px") ]


margin10 : List (Attribute msg)
margin10 =
    [ style "margin" "10px" ]


gap15 : List (Attribute msg)
gap15 =
    [ style "gap" "15px" ]
