module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
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


type alias BigBoard =
    { boards : List SmallBoard
    , currentPlayer : Player
    , activeBoard : Maybe Int  -- The board where the next move must be played (Nothing if player can choose any board)
    , winner : Maybe Player
    }


type BotDifficulty
    = Easy
    | Medium
    | Hard
    | Elite


type GameMode
    = WithFriend
    | WithBot BotDifficulty


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
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | CellClicked Int Int  -- First Int is board index (0-8), second is cell index (0-8)
    | RestartGame
    | StartGameWithFriend
    | StartGameWithBot
    | SelectBotDifficulty BotDifficulty
    | CancelBotDifficulty
    | ReturnToMenu
    | BotMove
    | ChangeLanguage Language


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend