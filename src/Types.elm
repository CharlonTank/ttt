port module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Url exposing (Url)



-- PORTS


port storeLocalStorage_ : String -> Cmd msg


port receiveLocalStorage_ : (String -> msg) -> Sub msg


port getLocalStorageValue_ : String -> Cmd msg


port receiveLocalStorageValue_ : (String -> msg) -> Sub msg


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


type alias Move =
    { boardIndex : Int
    , cellIndex : Int
    , player : Player
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


type alias FrontendModel =
    { key : Key
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
    , debuggerPosition : { x : Float, y : Float }
    , isDraggingDebugger : Bool
    , dragOffset : { x : Float, y : Float }
    , debuggerSize : { width : Float, height : Float }
    , isResizingDebugger : Bool
    , localStorageValues : Dict String String
    , selectedDifficulty : Maybe BotDifficulty
    , rulesModalVisible : Bool
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
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
    | ReceivedLocalStorage { language : String, darkMode : Bool }
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
    | PlayForMe
    | ToggleRulesModal


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend



-- HELPERS


languageToString : Language -> String
languageToString lang =
    case lang of
        FR ->
            "FR"

        EN ->
            "EN"


stringToLanguage : String -> Language
stringToLanguage str =
    case str of
        "EN" ->
            EN

        _ ->
            FR
