module Types exposing (..)

import Browser exposing (UrlRequest)
import Effect.Browser.Navigation exposing (Key)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import I18n exposing (Language(..), Translation, languageToString)
import Id exposing (GameId(..), Id(..))
import Lamdera.Json as Json
import LocalStorage exposing (LocalStorage)
import Random
import SeqDict as Dict exposing (SeqDict)
import Theme exposing (..)
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
    }


type BotDifficulty
    = Easy
    | Medium
    | Hard
    | Elite


type GameMode
    = WithFriend
    | WithBot BotDifficulty
    | OnlineGameMode


type Route
    = HomeRoute
    | GameRoute GameMode
    | AdminRoute
    | TutorialRoute


type GameResult
    = Won
    | Lost
    | Draw


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , route : Route
    , localStorage : LocalStorage
    , seed : Maybe Random.Seed
    , rulesModalVisible : Bool
    , frClickCount : Int
    , debuggerVisible : Bool
    , debuggerPosition : Position
    , isDraggingDebugger : Bool
    , dragOffset : Position
    , debuggerSize : Size
    , isResizingDebugger : Bool
    , selectedDifficulty : Maybe BotDifficulty
    , showAbandonConfirm : Bool
    , tutorialState : Maybe TutorialStep
    , botDifficultyMenuOpen : Bool
    , inMatchmaking : Bool
    , isLoading : Bool
    , backendModel : Maybe BackendModel
    , frontendGame : Maybe FrontendGame
    }


type alias UserConfig =
    { t : Translation
    , c : Theme
    }


type alias OnlineGame =
    { id : Id GameId
    , playerX : SessionId
    , playerO : SessionId
    , boards : List SmallBoard
    , currentPlayer : Player
    , activeBoard : Maybe Int
    , winner : Maybe Player
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    }


type Opponent
    = OnlineOpponent SessionId
    | BotOpponent BotDifficulty
    | FriendOpponent


type alias FrontendGame =
    { opponent : Opponent
    , boards : List SmallBoard
    , currentPlayer : Player
    , self : Maybe Player
    , activeBoard : Maybe Int
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    , winner : Maybe Player
    , gameResult : Maybe GameResult
    , botIsPlaying : Bool
    }


type alias SessionRecord =
    { clientIds : List ClientId
    , activity : Activity
    }


type Activity
    = InQueue
    | InGame (Id GameId)
    | Available


type alias BackendModel =
    { matchmakingQueue : List SessionId
    , activeGames : SeqDict (Id GameId) OnlineGame
    , finishedGames : SeqDict (Id GameId) OnlineGame
    , seed : Random.Seed
    , sessions : SeqDict SessionId SessionRecord
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | NoOp
    | CellClicked Move
    | BotMove
    | RestartGame
    | StartGameWithFriend
    | StartGameWithBot
    | SelectBotDifficulty BotDifficulty
    | StartBotGame BotDifficulty FirstPlayer
    | ReturnToMenu
    | CancelBotDifficulty
    | PlayForMeAgainstTheBot
    | ChangeLanguage Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleSound
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
    | GotTime Effect.Time.Posix
    | Tick Effect.Time.Posix
    | ShowAbandonConfirm
    | HideAbandonConfirm
    | ConfirmAbandon FrontendGame
    | StartTutorial
    | NextTutorialStep
    | CompleteTutorial
    | LeaveMatchmaking
    | LoadingComplete
    | KeyLeft
    | KeyRight


type FirstPlayer
    = HumanBegins
    | BotBegins
    | RandomBegins


localStorageToString : LocalStorage -> String
localStorageToString localStorage =
    "language: "
        ++ languageToString localStorage.language
        ++ "\n"
        ++ "userPreference: "
        ++ userPreferenceToString localStorage.userPreference localStorage.systemMode
        ++ "\n"


type ToBackend
    = NoOpToBackend
    | RequestBackendModelToBackend
    | JoinMatchmakingToBackend
    | LeaveMatchmakingToBackend
    | MakeMoveToBackend Move
    | AbandonGameToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | GameFoundToFrontend { opponentId : SessionId, playerRole : Player }
    | OpponentMoveToFrontend Move
    | OpponentLeftToFrontend FrontendGame
    | BackendModelReceivedToFrontend BackendModel
    | SendGameToFrontend FrontendGame



-- HELPERS


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }
