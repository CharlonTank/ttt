module Palette.Typography exposing (h1, h2, pre, text, loadingTitle, loadingSubtitle, loadingAuthor)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Theme exposing (Theme)
import Color


h1 : Theme -> String -> Html msg
h1 c content =
    Html.h1
        [ style "margin" "0 0 20px 0"
        , style "color" c.text
        , style "font-size" "1.5em"
        ]
        [ Html.text content ]


h2 : Theme -> String -> Html msg
h2 c content =
    Html.h2
        [ style "margin" "0 0 20px 0"
        , style "color" c.text
        , style "font-size" "1.2em"
        ]
        [ Html.text content ]


text : Theme -> String -> Html msg
text c content =
    Html.p
        [ style "color" c.text
        , style "line-height" "1.6"
        , style "font-size" "0.7em"
        ]
        [ Html.text content ]


pre : Theme -> String -> Html msg
pre c content =
    Html.pre
        [ style "color" c.text
        , style "white-space" "pre-wrap"
        , style "font-family" "inherit"
        , style "text-align" "left"
        , style "line-height" "1.6"
        , style "margin" "0 0 20px 0"
        , style "font-size" "0.7em"
        ]
        [ Html.text content ]


loadingTitle : Theme -> String -> Html msg
loadingTitle c content =
    Html.div
        [ style "font-size" "min(2.5em, 10vw)"
        , style "color" c.text
        , style "line-height" "1.2"
        ]
        [ Html.text content ]


loadingSubtitle : Theme -> String -> Html msg
loadingSubtitle c content =
    Html.div
        [ style "font-size" "min(1.5em, 6vw)"
        , style "color" c.text
        , style "margin-bottom" "15px"
        ]
        [ Html.text content ]


loadingAuthor : Theme -> String -> Html msg
loadingAuthor c content =
    Html.div
        [ style "font-size" "min(0.8em, 3vw)"
        , style "color" Color.primary
        , style "opacity" "0.8"
        , style "font-style" "italic"
        ]
        [ Html.text content ]
