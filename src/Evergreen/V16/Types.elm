module Evergreen.V16.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V16.Tutorial.Types
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


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
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
    , language : Language
    , darkMode : Bool
    , moveHistory : List Move
    , currentMoveIndex : Int
    , rulesModalVisible : Bool
    , humanPlaysFirst : Bool
    , frClickCount : Int
    , debuggerVisible : Bool
    , debuggerPosition : Position
    , isDraggingDebugger : Bool
    , dragOffset : Position
    , debuggerSize : Size
    , isResizingDebugger : Bool
    , localStorageValues : Dict.Dict String String
    , selectedDifficulty : Maybe BotDifficulty
    , onlinePlayer : Maybe Player
    , showAbandonConfirm : Bool
    , gameResult : GameResult
    , tutorialState : Maybe Evergreen.V16.Tutorial.Types.TutorialStep
    , botDifficultyMenuOpen : Bool
    , botThinking : Bool
    , inMatchmaking : Bool
    , onlineOpponent : Maybe Lamdera.ClientId
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
    | NoOp
    | CellClicked Int Int
    | BotMove
    | RestartGame
    | StartGameWithFriend
    | StartGameWithBot
    | SelectBotDifficulty BotDifficulty
    | StartWithPlayer Bool
    | ReturnToMenu
    | CancelBotDifficulty
    | PlayForMe
    | ChangeLanguage Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleDebugMode
    | ReceivedLocalStorage
        { language : String
        , darkMode : Bool
        }
    | StartDraggingDebugger Float Float
    | StopDraggingDebugger
    | DragDebugger Float Float
    | StartResizingDebugger
    | StopResizingDebugger
    | ResizeDebugger Float Float
    | ReceivedLocalStorageValue String String
    | ToggleRulesModal
    | StartOnlineGame
    | ReceivedGameFound
        { opponentId : Lamdera.ClientId
        , playerRole : Player
        }
    | ReceivedOpponentMove Move
    | ReceivedOpponentLeft
    | StartWithRandomPlayer
    | GotTime Time.Posix
    | Tick Time.Posix
    | ShowAbandonConfirm
    | HideAbandonConfirm
    | ConfirmAbandon
    | StartTutorial
    | NextTutorialStep
    | SkipTutorial
    | CompleteTutorial


type ToBackend
    = NoOpToBackend
    | JoinMatchmaking
    | LeaveMatchmaking
    | AbandonGame
    | MakeMove Int Int Player


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
