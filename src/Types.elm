port module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Lamdera exposing (ClientId)
import Url exposing (Url)
import Random
import Time



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
    , initialPlayer : Player
    }


type BotDifficulty
    = Easy
    | Medium
    | Hard
    | Elite


type GameMode
    = WithFriend
    | WithBot BotDifficulty
    | OnlineGame


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
    , inMatchmaking : Bool
    , onlineOpponent : Maybe ClientId
    , onlinePlayer : Maybe Player
    , showAbandonConfirm : Bool
    }


type alias BackendModel =
    { message : String
    , matchmakingQueue : List ClientId
    , activeGames : List ( ClientId, ClientId, BigBoard )
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | CellClicked Int Int
    | RestartGame
    | StartGameWithFriend
    | StartGameWithBot
    | StartOnlineGame
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
    | StartWithRandomPlayer
    | PlayForMe
    | ToggleRulesModal
    | ReceivedGameFound { opponentId : ClientId, playerRole : Player }
    | ReceivedOpponentMove Move
    | ReceivedOpponentLeft
    | GotTime Time.Posix
    | Tick Time.Posix
    | ShowAbandonConfirm
    | HideAbandonConfirm
    | ConfirmAbandon


type ToBackend
    = NoOpToBackend
    | JoinMatchmaking
    | LeaveMatchmaking
    | MakeMove Int Int Player


type BackendMsg
    = NoOpBackendMsg
    | GotInitialTime Time.Posix


type ToFrontend
    = NoOpToFrontend
    | GameFound { opponentId : ClientId, playerRole : Player }
    | OpponentMove Move
    | OpponentLeft



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
