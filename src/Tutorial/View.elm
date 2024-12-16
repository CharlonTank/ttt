module Tutorial.View exposing (..)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (circle, line, svg)
import Svg.Attributes
import Tutorial.Types exposing (TutorialStep(..))
import Types exposing (..)


viewTutorialCell : FrontendModel -> Int -> Int -> List (Html.Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewTutorialCell model boardIndex isClickableIndex cellStyles cellIndex cellState =
    let
        isTutorialCell =
            case model.tutorialState of
                Just step ->
                    case step of
                        TutorialBasicMove ->
                            boardIndex == 4 && cellIndex == 2

                        TutorialBoardSelection ->
                            False

                        TutorialWinningSmall ->
                            boardIndex == 4 && cellIndex == 4

                        TutorialWinningBig ->
                            boardIndex == 8 && cellIndex == 6

                        _ ->
                            True

                _ ->
                    False

        ( symbol, textColor, bgColor ) =
            case cellState of
                Empty ->
                    ( Html.text ""
                    , if model.darkMode then
                        Color.darkText

                      else
                        Color.lightText
                    , if model.darkMode then
                        Color.darkBackground

                      else
                        Color.lightBackground
                    )

                Filled X ->
                    ( div [ style "width" "100%", style "height" "100%", style "position" "relative" ]
                        [ svg
                            [ Svg.Attributes.viewBox "0 0 100 100"
                            , Svg.Attributes.width "100%"
                            , Svg.Attributes.height "100%"
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke Color.danger
                            , Svg.Attributes.strokeWidth "10"
                            , Svg.Attributes.strokeLinecap "round"
                            , style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            ]
                            [ line
                                [ Svg.Attributes.x1 "20"
                                , Svg.Attributes.y1 "20"
                                , Svg.Attributes.x2 "80"
                                , Svg.Attributes.y2 "80"
                                ]
                                []
                            , line
                                [ Svg.Attributes.x1 "80"
                                , Svg.Attributes.y1 "20"
                                , Svg.Attributes.x2 "20"
                                , Svg.Attributes.y2 "80"
                                ]
                                []
                            ]
                        ]
                    , Color.danger
                    , if model.darkMode then
                        Color.darkBackground

                      else
                        Color.lightBackground
                    )

                Filled O ->
                    ( div [ style "width" "100%", style "height" "100%", style "position" "relative" ]
                        [ svg
                            [ Svg.Attributes.viewBox "0 0 100 100"
                            , Svg.Attributes.width "100%"
                            , Svg.Attributes.height "100%"
                            , Svg.Attributes.fill "none"
                            , Svg.Attributes.stroke Color.primary
                            , Svg.Attributes.strokeWidth "10"
                            , Svg.Attributes.strokeLinecap "round"
                            , style "position" "absolute"
                            , style "top" "0"
                            , style "left" "0"
                            ]
                            [ circle
                                [ Svg.Attributes.cx "50"
                                , Svg.Attributes.cy "50"
                                , Svg.Attributes.r "35"
                                ]
                                []
                            ]
                        ]
                    , Color.primary
                    , if model.darkMode then
                        Color.darkBackground

                      else
                        Color.lightBackground
                    )

        isCellClickable =
            isClickableIndex == 1 && cellState == Empty && isTutorialCell

        cursor =
            if isCellClickable then
                "pointer"

            else
                "default"

        cornerRadius =
            case cellIndex of
                0 ->
                    [ style "border-top-left-radius" "4px" ]

                2 ->
                    [ style "border-top-right-radius" "4px" ]

                6 ->
                    [ style "border-bottom-left-radius" "4px" ]

                8 ->
                    [ style "border-bottom-right-radius" "4px" ]

                _ ->
                    []

        highlight =
            if isTutorialCell && cellState == Empty && isClickableIndex == 1 then
                [ style "box-shadow" "inset 0 0 0 2px #4CAF50"
                , style "animation" "blink 1s ease-in-out infinite"
                ]

            else
                []
    in
    div
        ([ style "width" "100%"
         , style "aspect-ratio" "1/1"
         , style "background-color" bgColor
         , style "display" "flex"
         , style "align-items" "center"
         , style "justify-content" "center"
         , style "cursor" cursor
         , style "color" textColor
         , style "user-select" "none"
         , style "position" "relative"
         ]
            ++ cornerRadius
            ++ cellStyles
            ++ highlight
            ++ (if isCellClickable then
                    [ onClick (CellClicked boardIndex cellIndex) ]

                else
                    []
               )
        )
        [ symbol ]
