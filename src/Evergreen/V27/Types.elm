module Evergreen.V27.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V27.I18n
import Evergreen.V27.Id
import Evergreen.V27.LocalStorage
import Evergreen.V27.Tutorial.Types
import Random
import SeqDict
import Url


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


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }


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


type alias OnlineGame =
    { id : Evergreen.V27.Id.Id Evergreen.V27.Id.GameId
    , playerX : Effect.Lamdera.SessionId
    , playerO : Effect.Lamdera.SessionId
    , boards : List SmallBoard
    , currentPlayer : Player
    , activeBoard : Maybe Int
    , winner : Maybe Player
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    }


type Activity
    = InQueue
    | InGame (Evergreen.V27.Id.Id Evergreen.V27.Id.GameId)
    | Available


type alias SessionRecord =
    { clientIds : List Effect.Lamdera.ClientId
    , activity : Activity
    }


type alias BackendModel =
    { matchmakingQueue : List Effect.Lamdera.SessionId
    , activeGames : SeqDict.SeqDict (Evergreen.V27.Id.Id Evergreen.V27.Id.GameId) OnlineGame
    , finishedGames : SeqDict.SeqDict (Evergreen.V27.Id.Id Evergreen.V27.Id.GameId) OnlineGame
    , seed : Random.Seed
    , sessions : SeqDict.SeqDict Effect.Lamdera.SessionId SessionRecord
    }


type Opponent
    = OnlineOpponent Effect.Lamdera.SessionId
    | BotOpponent BotDifficulty
    | FriendOpponent


type GameResult
    = Won
    | Lost
    | Draw


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


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , route : Route
    , localStorage : Evergreen.V27.LocalStorage.LocalStorage
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
    , tutorialState : Maybe Evergreen.V27.Tutorial.Types.TutorialStep
    , botDifficultyMenuOpen : Bool
    , inMatchmaking : Bool
    , isLoading : Bool
    , backendModel : Maybe BackendModel
    , frontendGame : Maybe FrontendGame
    }


type FirstPlayer
    = HumanBegins
    | BotBegins
    | RandomBegins


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | ChangeLanguage Evergreen.V27.I18n.Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleSound
    | ToggleDebugMode
    | ReceivedLocalStorage Evergreen.V27.LocalStorage.LocalStorage
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


type ToBackend
    = NoOpToBackend
    | RequestBackendModelToBackend
    | JoinMatchmakingToBackend
    | LeaveMatchmakingToBackend
    | MakeMoveToBackend Move
    | AbandonGameToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | GameFoundToFrontend
        { opponentId : Effect.Lamdera.SessionId
        , playerRole : Player
        }
    | OpponentMoveToFrontend Move
    | OpponentLeftToFrontend FrontendGame
    | BackendModelReceivedToFrontend BackendModel
    | SendGameToFrontend FrontendGame
