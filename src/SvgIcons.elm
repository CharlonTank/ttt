module SvgIcons exposing (eye, eyeClosed, o, play, x)

import Html exposing (Html)
import Svg
import Svg.Attributes exposing (..)


eye : String -> Html msg
eye color =
    Svg.svg
        [ viewBox "0 0 24 24"
        , width "24"
        , height "24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        ]
        [ Svg.path [ d "M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z" ] []
        , Svg.circle [ cx "12", cy "12", r "3" ] []
        ]


eyeClosed : String -> Html msg
eyeClosed color =
    Svg.svg
        [ viewBox "0 0 24 24"
        , width "24"
        , height "24"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        ]
        [ Svg.path [ d "M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]


x : String -> Html msg
x color =
    Svg.svg
        [ viewBox "0 0 100 100"
        , width "100%"
        , height "100%"
        , fill "none"
        , stroke color
        , strokeWidth "10"
        , strokeLinecap "round"
        ]
        [ Svg.line [ x1 "20", y1 "20", x2 "80", y2 "80" ] []
        , Svg.line [ x1 "80", y1 "20", x2 "20", y2 "80" ] []
        ]


o : String -> Html msg
o color =
    Svg.svg
        [ viewBox "0 0 100 100"
        , width "100%"
        , height "100%"
        , fill "none"
        , stroke color
        , strokeWidth "10"
        , strokeLinecap "round"
        ]
        [ Svg.circle [ cx "50", cy "50", r "35" ] [] ]


play : String -> Html msg
play color =
    Svg.svg
        [ viewBox "0 0 24 24"
        , width "48"
        , height "48"
        , fill "none"
        , stroke color
        , strokeWidth "2"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ Svg.path
            [ d "M7 4.5 C6 4 6 5 6 5.5 L6 18.5 C6 19.5 7 19.5 7.5 19 L17.5 12.5 C18.5 11.5 17.5 11 17 11 L7 4.5 Z"
            , fill color
            , stroke "none"
            ]
            []
        ]
