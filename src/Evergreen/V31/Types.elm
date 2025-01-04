module Evergreen.V31.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V31.I18n
import Evergreen.V31.Id
import Evergreen.V31.LocalStorage
import Evergreen.V31.Tutorial.Types
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
    { id : Evergreen.V31.Id.Id Evergreen.V31.Id.GameId
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


type alias BackendModel =
    { matchmakingQueue : List Effect.Lamdera.SessionId
    , activeGames : SeqDict.SeqDict (Evergreen.V31.Id.Id Evergreen.V31.Id.GameId) OnlineGame
    , finishedGames : SeqDict.SeqDict (Evergreen.V31.Id.Id Evergreen.V31.Id.GameId) OnlineGame
    , seed : Random.Seed
    , sessions : SeqDict.SeqDict Effect.Lamdera.SessionId (List Effect.Lamdera.ClientId)
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
    { id : Maybe (Evergreen.V31.Id.Id Evergreen.V31.Id.GameId)
    , opponent : Opponent
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
    , localStorage : Evergreen.V31.LocalStorage.LocalStorage
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
    , tutorialState : Maybe Evergreen.V31.Tutorial.Types.TutorialStep
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
    | PlayForMeAgainstTheBotClicked
    | PlayForMeAgainstTheBot
    | ChangeLanguage Evergreen.V31.I18n.Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleSound
    | ToggleDebugMode
    | ReceivedLocalStorage Evergreen.V31.LocalStorage.LocalStorage
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
    | AbandonGameToBackend (Evergreen.V31.Id.Id Evergreen.V31.Id.GameId)


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | OpponentLeftToFrontend FrontendGame
    | BackendModelReceivedToFrontend BackendModel
    | SendGameToFrontend FrontendGame
    | AlreadyInMatchmakingToFrontend
