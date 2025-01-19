module Types exposing (..)

import Browser exposing (UrlRequest)
import Effect.Browser.Navigation exposing (Key)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Time
import I18n exposing (Language(..), Translation, languageToString)
import Id exposing (GameId(..), Id(..), UserId)
import Lamdera.Json as Json
import LocalStorage exposing (LocalStorage)
import Random
import SeqDict as Dict exposing (SeqDict)
import Theme exposing (..)
import Tutorial.Types exposing (TutorialStep)
import Url exposing (Url)


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


type GameResult
    = Won
    | Lost
    | Draw


type LoginErrorWrapper
    = WrongPasswordError
    | PasswordTooShortError
    | InvalidEmailError


type LoginState
    = NotLoggedIn
    | WaitingForAnswer
    | LoginError LoginErrorWrapper
    | Registered


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , route : Route
    , localStorage : LocalStorage
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
    , tutorialState : Maybe TutorialStep
    , botDifficultyMenuOpen : Bool
    , inMatchmaking : Bool
    , isLoading : Bool
    , backendModel : Maybe BackendModel
    , frontendGame : Maybe FrontendGame
    , isPasswordVisible : Bool
    , loginEmail : String
    , loginPassword : String
    }


type alias UserConfig =
    { t : Translation
    , c : Theme
    }


type alias OnlineGameBackend =
    { id : Id GameId
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


type OfflineOpponent
    = BotOpponent BotDifficulty
    | FriendOpponent


type FrontendGame
    = OnlineGame FrontendOnlineGame
    | OfflineGame FrontendOfflineGame


type alias FrontendGameState state =
    { state
        | boards : List SmallBoard
        , currentPlayer : PlayerSide
        , self : Player
        , selfSide : PlayerSide
        , winner : Maybe PlayerSide
        , gameResult : Maybe GameResult
        , activeBoard : Maybe Int
        , lastMove : Maybe Move
        , moveHistory : List Move
        , currentMoveIndex : Int
    }


type alias FrontendOnlineGame =
    { id : Id GameId
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


type alias User =
    { id : Id UserId
    , email : Email
    , name : String
    , encryptedPassword : String
    , elo : Elo
    }


type alias PublicUser =
    { id : Id UserId
    , name : String
    , elo : Elo
    }


type alias Session =
    { userId : Maybe (Id UserId)
    , email : Maybe Email
    , clientIds : List ClientId
    }


type alias Elo =
    Int


type Player
    = Authenticated PublicUser
    | Anonymous SessionId Elo


type alias BackendModel =
    { matchmakingQueue : List Player
    , activeGames : SeqDict (Id GameId) OnlineGameBackend
    , finishedGames : SeqDict (Id GameId) OnlineGameBackend
    , seed : Random.Seed
    , sessions : SeqDict SessionId Session
    , users : SeqDict Email User
    }


type alias Email =
    String


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
    | PlayForMeAgainstTheBotClicked
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
    | NavigateToLogin
    | LoadingComplete
    | KeyLeft
    | KeyRight
    | TogglePasswordVisibility
    | LoginOrSignUpClicked
    | UpdateLoginEmail String
    | UpdateLoginPassword String
    | LogOut


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
    | AbandonGameToBackend (Id GameId)
    | LoginOrSignUpToBackend String String
    | LogOutToBackend


type BackendMsg
    = NoOpBackendMsg
    | ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


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



-- HELPERS


type alias Position =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }
