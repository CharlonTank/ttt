module Tutorial.View exposing (..)

import Color
import GameLogic exposing (isBigBoardComplete)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as List
import Svg exposing (circle, line, svg)
import Svg.Attributes
import Tutorial.Types exposing (TutorialStep(..))
import Types exposing (..)


viewTutorialCell : UserConfig -> TutorialStep -> Int -> Int -> List (Html.Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewTutorialCell { c } tutorialStep boardIndex isClickableIndex cellStyles cellIndex cellState =
    let
        isTutorialCell =
            case tutorialStep of
                TutorialStep1 ->
                    boardIndex == 4 && cellIndex == 2

                TutorialStep2 ->
                    False

                TutorialStep3 ->
                    boardIndex == 4 && cellIndex == 4

                TutorialStep5 ->
                    boardIndex == 8 && cellIndex == 6

                TutorialStep4 ->
                    True

                TutorialStep6 ->
                    False

        ( symbol, textColor, bgColor ) =
            case cellState of
                Empty ->
                    ( Html.text ""
                    , c.text
                    , c.background
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
                    , c.background
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
                    , c.background
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
                    [ onClick NextTutorialStep ]

                else
                    []
               )
        )
        [ symbol ]


viewSmallBoardTutorial : UserConfig -> TutorialStep -> FrontendOfflineGame -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoardTutorial ({ c } as userConfig) tutorialStep frontendGame boardIndex smallBoardData =
    let
        isActive =
            GameLogic.isSmallBoardActive frontendGame.activeBoard boardIndex smallBoardData

        isViewingHistory =
            frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1

        isTutorialBoard =
            case tutorialStep of
                TutorialStep1 ->
                    boardIndex == 4

                TutorialStep2 ->
                    True

                TutorialStep3 ->
                    boardIndex == 4

                TutorialStep4 ->
                    True

                TutorialStep5 ->
                    boardIndex == 8

                TutorialStep6 ->
                    False

        isClickable =
            isActive
                && not isViewingHistory
                && (case tutorialStep of
                        TutorialStep1 ->
                            isTutorialBoard

                        TutorialStep2 ->
                            False

                        TutorialStep3 ->
                            boardIndex == 4

                        TutorialStep4 ->
                            False

                        TutorialStep5 ->
                            isTutorialBoard

                        TutorialStep6 ->
                            False
                   )

        isClickableIndex =
            if isClickable then
                1

            else
                0

        borderColor =
            case tutorialStep of
                TutorialStep2 ->
                    if boardIndex == 2 then
                        Color.success

                    else
                        c.border

                _ ->
                    if isActive then
                        Color.success

                    else
                        c.border

        backgroundColor =
            case smallBoardData.winner of
                Just X ->
                    Color.playerX

                Just O ->
                    Color.playerO

                Nothing ->
                    if not isTutorialBoard then
                        Color.disabled

                    else
                        c.background

        cellStyle =
            [ style "box-shadow" ("inset 0 0 0 1px " ++ c.border) ]

        cellElements =
            List.indexedMap (viewTutorialCell userConfig tutorialStep boardIndex isClickableIndex cellStyle) smallBoardData.cells

        opacity =
            case tutorialStep of
                TutorialStep3 ->
                    if boardIndex /= 4 then
                        [ style "opacity" "0.5" ]

                    else
                        []

                _ ->
                    if not isTutorialBoard then
                        [ style "opacity" "0.5" ]

                    else
                        []
    in
    div
        ([ style "background-color" backgroundColor
         , style "border-radius" "8px"
         , style "display" "grid"
         , style "grid-template-columns" "repeat(3, 1fr)"
         , style "gap" "0"
         , style "padding" "4px"
         , style "aspect-ratio" "1/1"
         , style "box-shadow" ("0 0 0 2px " ++ borderColor)
         ]
            ++ opacity
        )
        cellElements


viewTutorialOverlay : UserConfig -> TutorialStep -> Html FrontendMsg
viewTutorialOverlay { t, c } tutorialStep =
    let
        ( tutorialMessage, stepNumber ) =
            case tutorialStep of
                TutorialStep1 ->
                    ( t.tutorialBasicMove, 1 )

                TutorialStep2 ->
                    ( t.tutorialBoardSelection, 2 )

                TutorialStep3 ->
                    ( t.tutorialWinningSmall, 3 )

                TutorialStep4 ->
                    ( t.tutorialFreeChoice, 4 )

                TutorialStep5 ->
                    ( t.tutorialWinningBig, 5 )

                TutorialStep6 ->
                    ( t.tutorialComplete, 6 )

        overlayStyles =
            case tutorialStep of
                TutorialStep5 ->
                    [ style "bottom" "70%"
                    , style "transform" "translate(-50%, 0%)"
                    ]

                _ ->
                    [ style "bottom" "0px"
                    , style "left" "0"
                    , style "right" "0"
                    , style "margin-left" "auto"
                    , style "margin-right" "auto"
                    ]

        buttons =
            if shouldEnableNextButton tutorialStep then
                [ button
                    [ style "padding" "10px 20px"
                    , style "font-size" "0.9em"
                    , style "background-color" Color.primary
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    , style "cursor" "pointer"
                    , style "transition" "all 0.2s ease"
                    , onClick NextTutorialStep
                    ]
                    [ text t.nextStep ]
                ]

            else
                []
    in
    div
        ([ style "position" "fixed"
         , style "left" "50%"
         , style "background-color" (Color.withAlpha c.background 0.85)
         , style "padding" "20px"
         , style "border-radius" "15px"
         , style "box-shadow" "0 4px 12px rgba(0, 0, 0, 0.2)"
         , style "max-width" "90%"
         , style "width" "600px"
         , style "text-align" "center"
         , style "animation" "slideIn 0.3s ease-out"
         , style "z-index" "1000"
         , style "backdrop-filter" "blur(5px)"
         ]
            ++ overlayStyles
        )
        [ div
            [ style "margin-bottom" "10px"
            , style "color" c.text
            , style "font-size" "0.8em"
            ]
            [ text (String.fromInt stepNumber ++ "/6") ]
        , p
            [ style "margin" "0 0 15px 0"
            , style "color" c.text
            , style "font-size" "1em"
            , style "line-height" "1.5"
            ]
            [ text tutorialMessage ]
        , div
            [ style "display" "flex"
            , style "gap" "10px"
            , style "justify-content" "center"
            ]
            buttons
        ]


shouldEnableNextButton : TutorialStep -> Bool
shouldEnableNextButton step =
    case step of
        TutorialStep1 ->
            False

        TutorialStep2 ->
            True

        TutorialStep3 ->
            False

        TutorialStep4 ->
            True

        TutorialStep5 ->
            False

        TutorialStep6 ->
            False


viewGame : UserConfig -> FrontendOfflineGame -> TutorialStep -> Html FrontendMsg
viewGame ({ t, c } as userConfig) frontendGame tutorialStep =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "padding-top" "20px"
        , style "padding-bottom" "20px"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "width" "100%"
        , style "max-width" "100vh"
        , style "margin" "auto"
        , style "box-sizing" "border-box"
        , style "position" "relative"
        , style "height" "100%"
        , style "background-color" c.background
        , style "color" c.text
        ]
        [ div
            [ style "text-align" "center"
            , style "padding" "10px"
            , style "flex-shrink" "0"
            ]
            [ text t.tutorialTitle
            , viewStatus userConfig frontendGame
            ]
        , div
            [ style "flex" "1"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "min-height" "0"
            , style "padding" "10px"
            , style "position" "relative"
            ]
            [ div
                [ style "width" "min(100%, calc(100vh - 300px))"
                , style "aspect-ratio" "1/1"
                ]
                [ viewBigBoardTutorial userConfig frontendGame tutorialStep ]
            ]
        ]


viewStatus : UserConfig -> FrontendGameState a -> Html msg
viewStatus ({ t, c } as userConfig) frontendGame =
    div
        [ style "margin" "10px 0"
        , style "color" c.text
        , style "font-size" "0.7em"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "gap" "10px"
            ]
            [ text <|
                case frontendGame.gameResult of
                    Just Won ->
                        t.youWon

                    Just Lost ->
                        t.youLost

                    Just Draw ->
                        t.draw

                    Nothing ->
                        if frontendGame.currentPlayer == X then
                            t.playerXTurn

                        else
                            t.playerOTurn
            ]
        ]


viewBigBoardTutorial : UserConfig -> FrontendOfflineGame -> TutorialStep -> Html FrontendMsg
viewBigBoardTutorial ({ t, c } as userConfig) frontendGame tutorialStep =
    let
        boardStyle =
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(3, 1fr)"
            , style "gap" "4px"
            , style "width" "100%"
            , style "aspect-ratio" "1/1"
            , style "margin" "0 auto"
            , style "padding" "4px"
            , style "background-color" c.border
            , style "border-radius" "12px"
            ]

        boardElements =
            List.indexedMap (viewSmallBoardTutorial userConfig tutorialStep frontendGame) frontendGame.boards
    in
    div
        boardStyle
        boardElements
