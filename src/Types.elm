module Types exposing (..)

import Browser exposing (UrlRequest)
import Dict exposing (Dict)
import Effect.Browser.Navigation exposing (Key)
import Effect.Lamdera exposing (ClientId)
import Effect.Time
import I18n exposing (Language(..), Translation, languageToString)
import Lamdera.Json as Json
import Random
import Theme exposing (DarkOrLight(..), Theme, darkModeToString)
import Tutorial.Types exposing (TutorialStep)
import Url exposing (Url)


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


type GameResult
    = Won
    | Lost
    | Drew
    | Ongoing


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , board : BigBoard
    , route : Route
    , language : Language
    , darkMode : DarkOrLight
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
    , localStorage : Maybe LocalStorage
    , selectedDifficulty : Maybe BotDifficulty
    , onlinePlayer : Maybe Player
    , showAbandonConfirm : Bool
    , gameResult : GameResult
    , tutorialState : Maybe TutorialStep
    , botDifficultyMenuOpen : Bool
    , botThinking : Bool
    , inMatchmaking : Bool
    , onlineOpponent : Maybe ClientId
    }

type alias UserConfig =
    { t : Translation
    , c : Theme
    }


type alias BackendModel =
    { message : String
    , matchmakingQueue : List ClientId
    , activeGames : List ( ClientId, ClientId, BigBoard )
    , seed : Random.Seed
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
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
    | ReceivedLocalStorage LocalStorage
    | StartDraggingDebugger Float Float
    | StopDraggingDebugger
    | DragDebugger Float Float
    | StartResizingDebugger
    | StopResizingDebugger
    | ResizeDebugger Float Float
    | ToggleRulesModal
    | StartOnlineGame
    | StartWithRandomPlayer
    | GotTime Effect.Time.Posix
    | Tick Effect.Time.Posix
    | ShowAbandonConfirm
    | HideAbandonConfirm
    | ConfirmAbandon
    | StartTutorial
    | NextTutorialStep
    | CompleteTutorial
    | LeaveMatchmaking


type alias LocalStorage =
    { language : Language
    , darkMode : DarkOrLight
    }


localStorageToString : LocalStorage -> String
localStorageToString localStorage =
    "language: "
        ++ languageToString localStorage.language
        ++ "\n"
        ++ "darkMode: "
        ++ darkModeToString localStorage.darkMode
        ++ "\n"


type ToBackend
    = NoOpToBackend
    | JoinMatchmaking
    | LeaveMatchmakingToBackend
    | AbandonGame
    | MakeMove Int Int Player


type BackendMsg
    = NoOpBackendMsg
    | GotInitialTime Effect.Time.Posix
    | PlayerDisconnected Effect.Lamdera.SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | GameFound { opponentId : ClientId, playerRole : Player }
    | OpponentMove Move
    | OpponentLeft



-- HELPERS


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }
