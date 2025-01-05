module Evergreen.V32.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V32.I18n
import Evergreen.V32.Id
import Evergreen.V32.LocalStorage
import Evergreen.V32.Tutorial.Types
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
    | LoginRoute


type LoginErrorWrapper
    = WrongPasswordError
    | PasswordTooShortError
    | InvalidEmailError


type alias Email =
    String


type alias PublicUser =
    { email : Email
    , name : String
    , elo : Int
    }


type LoginState
    = NotLoggedIn
    | WaitingForAnswer
    | LoginError LoginErrorWrapper
    | Registered PublicUser


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
    { id : Evergreen.V32.Id.Id Evergreen.V32.Id.GameId
    , playerX : Effect.Lamdera.SessionId
    , playerO : Effect.Lamdera.SessionId
    , boards : List SmallBoard
    , currentPlayer : Player
    , activeBoard : Maybe Int
    , winner : Maybe Player
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    , eloX : Int
    , eloO : Int
    }


type alias Session =
    { email : Maybe Email
    , clientIds : List Effect.Lamdera.ClientId
    , elo : Int
    }


type alias User =
    { email : Email
    , name : String
    , encryptedPassword : String
    , elo : Int
    }


type alias BackendModel =
    { matchmakingQueue : List Effect.Lamdera.SessionId
    , activeGames : SeqDict.SeqDict (Evergreen.V32.Id.Id Evergreen.V32.Id.GameId) OnlineGame
    , finishedGames : SeqDict.SeqDict (Evergreen.V32.Id.Id Evergreen.V32.Id.GameId) OnlineGame
    , seed : Random.Seed
    , sessions : SeqDict.SeqDict Effect.Lamdera.SessionId Session
    , users : SeqDict.SeqDict Email User
    }


type Opponent
    = OnlineOpponent ( Effect.Lamdera.SessionId, Int )
    | BotOpponent BotDifficulty
    | FriendOpponent


type GameResult
    = Won
    | Lost
    | Draw


type alias FrontendGame =
    { id : Maybe (Evergreen.V32.Id.Id Evergreen.V32.Id.GameId)
    , opponent : Opponent
    , boards : List SmallBoard
    , currentPlayer : Player
    , self : Maybe ( Player, Int )
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
    , localStorage : Evergreen.V32.LocalStorage.LocalStorage
    , seed : Maybe Random.Seed
    , login : LoginState
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
    , tutorialState : Maybe Evergreen.V32.Tutorial.Types.TutorialStep
    , botDifficultyMenuOpen : Bool
    , inMatchmaking : Bool
    , isLoading : Bool
    , backendModel : Maybe BackendModel
    , frontendGame : Maybe FrontendGame
    , isPasswordVisible : Bool
    , loginEmail : String
    , loginPassword : String
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
    | ChangeLanguage Evergreen.V32.I18n.Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleSound
    | ToggleDebugMode
    | ReceivedLocalStorage Evergreen.V32.LocalStorage.LocalStorage
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
    | NavigateToLogin
    | LoadingComplete
    | KeyLeft
    | KeyRight
    | TogglePasswordVisibility
    | LoginOrSignUpClicked
    | UpdateLoginEmail String
    | UpdateLoginPassword String
    | LogOut


type ToBackend
    = NoOpToBackend
    | RequestBackendModelToBackend
    | JoinMatchmakingToBackend
    | LeaveMatchmakingToBackend
    | MakeMoveToBackend Move
    | AbandonGameToBackend (Evergreen.V32.Id.Id Evergreen.V32.Id.GameId)
    | LoginOrSignUpToBackend String String
    | LogOutToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | OpponentLeftToFrontend FrontendGame
    | BackendModelReceivedToFrontend BackendModel
    | SendGameToFrontend FrontendGame
    | SendFinishedGameToFrontend FrontendGame
    | AlreadyInMatchmakingToFrontend
    | SendUserToFrontend (Maybe PublicUser)
    | SignUpDone PublicUser
    | SignInDone PublicUser
    | WrongPassword LoginErrorWrapper
