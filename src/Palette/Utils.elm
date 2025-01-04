module Palette.Utils exposing (fullWidth, gap15, margin10, maxWidth)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


fullWidth : Attribute msg
fullWidth =
    style "width" "100%"


maxWidth : Int -> Attribute msg
maxWidth px =
    style "max-width" (String.fromInt px ++ "px")


margin10 : Attribute msg
margin10 =
    style "margin" "10px"


gap15 : Attribute msg
gap15 =
    style "gap" "15px"
