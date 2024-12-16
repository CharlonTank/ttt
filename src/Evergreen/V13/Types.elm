module Evergreen.V13.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Random
import Time
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


type alias Move =
    { boardIndex : Int
    , cellIndex : Int
    , player : Player
    }


type GameResult
    = Won
    | Lost
    | Drew
    | Ongoing


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
    , rulesModalVisible : Bool
    , inMatchmaking : Bool
    , onlineOpponent : Maybe Lamdera.ClientId
    , onlinePlayer : Maybe Player
    , showAbandonConfirm : Bool
    , gameResult : GameResult
    }


type alias BackendModel =
    { message : String
    , matchmakingQueue : List Lamdera.ClientId
    , activeGames : List ( Lamdera.ClientId, Lamdera.ClientId, BigBoard )
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | StartWithRandomPlayer
    | PlayForMe
    | ToggleRulesModal
    | ReceivedGameFound
        { opponentId : Lamdera.ClientId
        , playerRole : Player
        }
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
    | AbandonGame


type BackendMsg
    = NoOpBackendMsg
    | GotInitialTime Time.Posix


type ToFrontend
    = NoOpToFrontend
    | GameFound
        { opponentId : Lamdera.ClientId
        , playerRole : Player
        }
    | OpponentMove Move
    | OpponentLeft
