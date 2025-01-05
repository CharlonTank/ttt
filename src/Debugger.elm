module Debugger exposing (view)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (languageToString)
import Json.Decode as D
import LocalStorage exposing (LocalStorage)
import Theme exposing (..)
import Tutorial.Types exposing (TutorialStep(..))
import Types exposing (..)


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
                , model.localStorage |> localStorageToString |> text
                , text "\nModel State:\n"
                , text <| "language: " ++ (model.localStorage.language |> languageToString) ++ "\n"
                , text <| "userPreference: " ++ userPreferenceToString model.localStorage.userPreference model.localStorage.systemMode ++ "\n"
                , text <| "route: " ++ routeToString model.route ++ "\n"
                , text <| "login: " ++ loginStateToString model.login ++ "\n"
                , text <| "selectedDifficulty: " ++ difficultyToString model.selectedDifficulty ++ "\n"
                , text <| "tutorialState: " ++ tutorialStateToString model.tutorialState ++ "\n"
                , text <| "inMatchmaking: " ++ boolToString model.inMatchmaking ++ "\n"
                , text <| "isLoading: " ++ boolToString model.isLoading ++ "\n\n"
                , text "Game State:\n"
                , model.frontendGame |> Maybe.map gameToString |> Maybe.withDefault "No frontend game found" |> text
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
                ]
                []
            ]
        ]

    else
        []


routeToString : Route -> String
routeToString route =
    case route of
        HomeRoute ->
            "HomeRoute"

        GameRoute mode ->
            "GameRoute " ++ gameModeToString mode

        TutorialRoute ->
            "TutorialRoute"

        AdminRoute ->
            "AdminRoute"

        LoginRoute ->
            "LoginRoute"


gameModeToString : GameMode -> String
gameModeToString mode =
    case mode of
        WithBot difficulty ->
            "WithBot " ++ difficultyToString (Just difficulty)

        WithFriend ->
            "WithFriend"

        OnlineGameMode ->
            "OnlineGameMode"


loginStateToString : LoginState -> String
loginStateToString state =
    case state of
        NotLoggedIn ->
            "NotLoggedIn"

        WaitingForAnswer ->
            "WaitingForAnswer"

        LoginError error ->
            "LoginError " ++ loginErrorToString error

        Registered user ->
            "Registered { email = " ++ user.email ++ ", elo = " ++ String.fromInt user.elo ++ " }"


loginErrorToString : LoginErrorWrapper -> String
loginErrorToString error =
    case error of
        WrongPasswordError ->
            "WrongPasswordError"

        PasswordTooShortError ->
            "PasswordTooShortError"

        InvalidEmailError ->
            "InvalidEmailError"


difficultyToString : Maybe BotDifficulty -> String
difficultyToString maybeDifficulty =
    case maybeDifficulty of
        Just difficulty ->
            case difficulty of
                Easy ->
                    "Easy"

                Medium ->
                    "Medium"

                Hard ->
                    "Hard"

                Elite ->
                    "Elite"

        Nothing ->
            "Nothing"


tutorialStateToString : Maybe TutorialStep -> String
tutorialStateToString maybeStep =
    case maybeStep of
        Just step ->
            case step of
                TutorialStep1 ->
                    "TutorialStep1"

                TutorialStep2 ->
                    "TutorialStep2"

                TutorialStep3 ->
                    "TutorialStep3"

                TutorialStep4 ->
                    "TutorialStep4"

                TutorialStep5 ->
                    "TutorialStep5"

                TutorialStep6 ->
                    "TutorialStep6"

        Nothing ->
            "Nothing"


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


gameToString : FrontendGame -> String
gameToString game =
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

        moveToString : Move -> String
        moveToString move =
            "{ boardIndex = "
                ++ String.fromInt move.boardIndex
                ++ ", cellIndex = "
                ++ String.fromInt move.cellIndex
                ++ " }"
    in
    "{ boards = ["
        ++ String.join ",\n  " (List.map smallBoardToString game.boards)
        ++ "],\n  currentPlayer = "
        ++ (case game.currentPlayer of
                X ->
                    "X"

                O ->
                    "O"
           )
        ++ ",\n  activeBoard = "
        ++ (case game.activeBoard of
                Nothing ->
                    "Nothing"

                Just n ->
                    "Just " ++ String.fromInt n
           )
        ++ ",\n  winner = "
        ++ (case game.winner of
                Nothing ->
                    "Nothing"

                Just player ->
                    case player of
                        X ->
                            "Just X"

                        O ->
                            "Just O"
           )
        ++ ",\n  moveHistory = ["
        ++ String.join ", " (List.map moveToString game.moveHistory)
        ++ "],\n  currentMoveIndex = "
        ++ String.fromInt game.currentMoveIndex
        ++ " }"


localStorageToString : LocalStorage -> String
localStorageToString localStorage =
    "language: "
        ++ languageToString localStorage.language
        ++ "\n"
        ++ "userPreference: "
        ++ userPreferenceToString localStorage.userPreference localStorage.systemMode
        ++ "\n"
