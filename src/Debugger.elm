module Debugger exposing (view)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (languageToString)
import Json.Decode as D
import Theme exposing (darkModeToString)
import Types exposing (..)


boardToString : BigBoard -> String
boardToString board =
    let
        cellStateToString : CellState -> String
        cellStateToString state =
            case state of
                Empty ->
                    "Empty"

                Filled player ->
                    case player of
                        X ->
                            "X"

                        O ->
                            "O"

        smallBoardToString : SmallBoard -> String
        smallBoardToString smallBoard =
            "{ cells = ["
                ++ String.join ", " (List.map cellStateToString smallBoard.cells)
                ++ "], winner = "
                ++ (case smallBoard.winner of
                        Nothing ->
                            "Nothing"

                        Just player ->
                            case player of
                                X ->
                                    "Just X"

                                O ->
                                    "Just O"
                   )
                ++ " }"
    in
    "{ boards = ["
        ++ String.join ",\n  " (List.map smallBoardToString board.boards)
        ++ "],\n  currentPlayer = "
        ++ (case board.currentPlayer of
                X ->
                    "X"

                O ->
                    "O"
           )
        ++ ",\n  activeBoard = "
        ++ (case board.activeBoard of
                Nothing ->
                    "Nothing"

                Just n ->
                    "Just " ++ String.fromInt n
           )
        ++ ",\n  winner = "
        ++ (case board.winner of
                Nothing ->
                    "Nothing"

                Just player ->
                    case player of
                        X ->
                            "Just X"

                        O ->
                            "Just O"
           )
        ++ " }"


view : UserConfig -> FrontendModel -> List (Html FrontendMsg)
view { c } model =
    if model.frClickCount >= 4 || model.debuggerVisible then
        [ div
            [ style "position" "fixed"
            , style "left" (String.fromFloat model.debuggerPosition.x ++ "px")
            , style "top" (String.fromFloat model.debuggerPosition.y ++ "px")
            , style "width" (String.fromFloat model.debuggerSize.width ++ "px")
            , style "height" (String.fromFloat model.debuggerSize.height ++ "px")
            , style "padding" "15px"
            , style "background-color" c.tertiaryBackground
            , style "color" c.text
            , style "border-radius" "10px"
            , style "font-family" "monospace"
            , style "font-size" "0.9em"
            , style "white-space" "pre-wrap"
            , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "overflow" "hidden"
            , style "touch-action" "none"
            , Html.Events.preventDefaultOn "touchmove" (D.succeed ( NoOp, True ))
            ]
            [ div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "align-items" "center"
                , style "margin-bottom" "10px"
                , style "user-select" "none"
                , style "cursor"
                    (if model.isDraggingDebugger then
                        "grabbing"

                     else
                        "grab"
                    )
                , Html.Events.on "mousedown"
                    (D.map2 StartDraggingDebugger
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                , Html.Events.preventDefaultOn "touchstart"
                    (D.map2
                        (\x y -> ( StartDraggingDebugger x y, True ))
                        (D.at [ "touches", "0", "clientX" ] D.float)
                        (D.at [ "touches", "0", "clientY" ] D.float)
                    )
                , Html.Events.preventDefaultOn "touchmove"
                    (D.map2
                        (\x y -> ( DragDebugger x y, True ))
                        (D.at [ "touches", "0", "clientX" ] D.float)
                        (D.at [ "touches", "0", "clientY" ] D.float)
                    )
                , Html.Events.preventDefaultOn "touchend"
                    (D.succeed ( StopDraggingDebugger, True ))
                ]
                [ strong [] [ text "Debugger" ]
                , button
                    [ style "padding" "4px 8px"
                    , style "background-color" Color.danger
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "4px"
                    , style "cursor" "pointer"
                    , onClick CloseDebugger
                    ]
                    [ text "×" ]
                ]
            , div
                [ style "flex" "1"
                , style "overflow" "auto"
                , style "padding-right" "10px"
                ]
                [ text "Local Storage:\n"
                , model.localStorage |> Maybe.map localStorageToString |> Maybe.withDefault "" |> text
                , text "\nModel State:\n"
                , text <| "language: " ++ (model.language |> languageToString) ++ "\n"
                , text <| "darkMode: " ++ (model.darkMode |> darkModeToString) ++ "\n\n"
                , text "Game State:\n"
                , text <| boardToString model.board
                , text "\n\nMove History:\n"
                , text <| "Current Move Index: " ++ String.fromInt model.currentMoveIndex ++ "\n"
                , text <| "Total Moves: " ++ String.fromInt (List.length model.moveHistory) ++ "\n"
                , text "History:\n"
                , List.indexedMap
                    (\i move ->
                        text <|
                            String.fromInt i
                                ++ ": "
                                ++ (if i == model.currentMoveIndex then
                                        "→ "

                                    else
                                        "  "
                                   )
                                ++ "Board "
                                ++ String.fromInt move.boardIndex
                                ++ ", Cell "
                                ++ String.fromInt move.cellIndex
                                ++ " ("
                                ++ (case move.player of
                                        X ->
                                            "X"

                                        O ->
                                            "O"
                                   )
                                ++ ")\n"
                    )
                    model.moveHistory
                    |> div []
                ]
            , div
                [ style "position" "absolute"
                , style "right" "0"
                , style "bottom" "0"
                , style "width" "20px"
                , style "height" "20px"
                , style "cursor" "se-resize"
                , style "background-image" "linear-gradient(135deg, transparent 50%, rgba(127, 127, 127, 0.3) 50%)"
                , style "border-bottom-right-radius" "8px"
                , Html.Events.onMouseDown StartResizingDebugger
                , Html.Events.preventDefaultOn "touchstart"
                    (D.succeed ( StartResizingDebugger, True ))
                , Html.Events.preventDefaultOn "touchmove"
                    (D.map2
                        (\x y -> ( ResizeDebugger x y, True ))
                        (D.at [ "touches", "0", "clientX" ] D.float)
                        (D.at [ "touches", "0", "clientY" ] D.float)
                    )
                , Html.Events.preventDefaultOn "touchend"
                    (D.succeed ( StopResizingDebugger, True ))
                ]
                []
            ]
        ]

    else
        []


localStorageToString : LocalStorage -> String
localStorageToString localStorage =
    "language: "
        ++ languageToString (Just localStorage.language)
        ++ "\n"
        ++ "darkMode: "
        ++ darkModeToString localStorage.darkMode
        ++ "\n"
