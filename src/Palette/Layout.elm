module Palette.Layout exposing (flexCenter, flexCenterWithGap, flexColumnCenter)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


flexCenter : List (Attribute msg)
flexCenter =
    [ style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]


flexCenterWithGap : List (Attribute msg)
flexCenterWithGap =
    flexCenter
        ++ [ style "gap" "10px"
           ]


flexColumnCenter : List (Attribute msg)
flexColumnCenter =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    ]
