module Evergreen.V8.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Url


type Player
    = X
    | O


type CellState
    = Empty
    | Filled Player


type alias SmallBoard =
    { cells : List CellState
    , winner : Maybe Player
    }


type alias BigBoard =
    { boards : List SmallBoard
    , currentPlayer : Player
    , activeBoard : Maybe Int
    , winner : Maybe Player
    }


type BotDifficulty
    = Easy
    | Medium
    | Hard
    | Elite


type GameMode
    = WithFriend
    | WithBot BotDifficulty


type Route
    = Home
    | Game GameMode


type Language
    = FR
    | EN


type alias Move =
    { boardIndex : Int
    , cellIndex : Int
    , player : Player
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , board : BigBoard
    , route : Route
    , botDifficultyMenuOpen : Bool
    , language : Language
    , botThinking : Bool
    , moveHistory : List Move
    , currentMoveIndex : Int
    , darkMode : Bool
    , humanPlaysFirst : Bool
    , frClickCount : Int
    , debuggerVisible : Bool
    , debuggerPosition :
        { x : Float
        , y : Float
        }
    , isDraggingDebugger : Bool
    , dragOffset :
        { x : Float
        , y : Float
        }
    , debuggerSize :
        { width : Float
        , height : Float
        }
    , isResizingDebugger : Bool
    , localStorageValues : Dict.Dict String String
    , selectedDifficulty : Maybe BotDifficulty
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CellClicked Int Int
    | RestartGame
    | StartGameWithFriend
    | StartGameWithBot
    | SelectBotDifficulty BotDifficulty
    | CancelBotDifficulty
    | ReturnToMenu
    | BotMove
    | ChangeLanguage Language
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ReceivedLocalStorage
        { language : String
        , darkMode : Bool
        }
    | ToggleDebugMode
    | CloseDebugger
    | StartDraggingDebugger Float Float
    | StopDraggingDebugger
    | DragDebugger Float Float
    | StartResizingDebugger
    | StopResizingDebugger
    | ResizeDebugger Float Float
    | NoOp
    | ReceivedLocalStorageValue String String
    | StartWithPlayer Bool


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
