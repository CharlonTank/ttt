module Evergreen.V35.Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Effect.Lamdera
import Effect.Time
import Evergreen.V35.I18n
import Evergreen.V35.Id
import Evergreen.V35.LocalStorage
import Evergreen.V35.Tutorial.Types
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


type LoginState
    = NotLoggedIn
    | WaitingForAnswer
    | LoginError LoginErrorWrapper
    | Registered


type alias Elo =
    Int


type alias PublicUser =
    { id : Evergreen.V35.Id.Id Evergreen.V35.Id.UserId
    , name : String
    , elo : Elo
    }


type Player
    = Authenticated PublicUser
    | Anonymous Effect.Lamdera.SessionId Elo


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }


type PlayerSide
    = X
    | O


type CellState
    = Empty
    | Filled PlayerSide


type alias SmallBoard =
    { cells : List CellState
    , winner : Maybe PlayerSide
    }


type alias Move =
    { boardIndex : Int
    , cellIndex : Int
    }


type alias OnlineGameBackend =
    { id : Evergreen.V35.Id.Id Evergreen.V35.Id.GameId
    , playerX : Player
    , playerO : Player
    , boards : List SmallBoard
    , currentPlayer : PlayerSide
    , activeBoard : Maybe Int
    , winner : Maybe PlayerSide
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    }


type alias Email =
    String


type alias Session =
    { userId : Maybe (Evergreen.V35.Id.Id Evergreen.V35.Id.UserId)
    , email : Maybe Email
    , clientIds : List Effect.Lamdera.ClientId
    }


type alias User =
    { id : Evergreen.V35.Id.Id Evergreen.V35.Id.UserId
    , email : Email
    , name : String
    , encryptedPassword : String
    , elo : Elo
    }


type alias BackendModel =
    { matchmakingQueue : List Player
    , activeGames : SeqDict.SeqDict (Evergreen.V35.Id.Id Evergreen.V35.Id.GameId) OnlineGameBackend
    , finishedGames : SeqDict.SeqDict (Evergreen.V35.Id.Id Evergreen.V35.Id.GameId) OnlineGameBackend
    , seed : Random.Seed
    , sessions : SeqDict.SeqDict Effect.Lamdera.SessionId Session
    , users : SeqDict.SeqDict Email User
    }


type GameResult
    = Won
    | Lost
    | Draw


type alias FrontendOnlineGame =
    { id : Evergreen.V35.Id.Id Evergreen.V35.Id.GameId
    , opponent : Player
    , boards : List SmallBoard
    , currentPlayer : PlayerSide
    , self : Player
    , selfSide : PlayerSide
    , activeBoard : Maybe Int
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    , winner : Maybe PlayerSide
    , gameResult : Maybe GameResult
    }


type OfflineOpponent
    = BotOpponent BotDifficulty
    | FriendOpponent


type alias FrontendOfflineGame =
    { opponent : OfflineOpponent
    , boards : List SmallBoard
    , currentPlayer : PlayerSide
    , self : Player
    , selfSide : PlayerSide
    , activeBoard : Maybe Int
    , lastMove : Maybe Move
    , moveHistory : List Move
    , currentMoveIndex : Int
    , winner : Maybe PlayerSide
    , gameResult : Maybe GameResult
    , botIsPlaying : Bool
    }


type FrontendGame
    = OnlineGame FrontendOnlineGame
    | OfflineGame FrontendOfflineGame


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , route : Route
    , localStorage : Evergreen.V35.LocalStorage.LocalStorage
    , seed : Maybe Random.Seed
    , login : LoginState
    , self : Maybe Player
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
    , tutorialState : Maybe Evergreen.V35.Tutorial.Types.TutorialStep
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
    | ChangeLanguage Evergreen.V35.I18n.Language
    | CloseDebugger
    | UndoMove
    | RedoMove
    | ToggleDarkMode
    | ToggleSound
    | ToggleDebugMode
    | ReceivedLocalStorage Evergreen.V35.LocalStorage.LocalStorage
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
    | AbandonGameToBackend (Evergreen.V35.Id.Id Evergreen.V35.Id.GameId)
    | LoginOrSignUpToBackend String String
    | LogOutToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId
    | ClientDisconnected Effect.Lamdera.SessionId Effect.Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | OpponentLeftToFrontend FrontendOnlineGame
    | BackendModelReceivedToFrontend BackendModel
    | SendGameToFrontend FrontendOnlineGame
    | SendFinishedGameToFrontend FrontendOnlineGame
    | JoinMatchmakingToFrontend
    | LeftMatchmakingToFrontend
    | SendUserToFrontend Player
    | SignUpDone PublicUser
    | SignInDone PublicUser
    | WrongPassword LoginErrorWrapper
