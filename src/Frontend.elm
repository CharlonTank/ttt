module Frontend exposing (..)

import Audio
import Bot
import Browser exposing (UrlRequest(..))
import Color
import Debugger
import Dict exposing (Dict)
import Duration
import Effect.Browser.Dom as Dom exposing (HtmlId)
import Effect.Browser.Events
import Effect.Browser.Navigation as Nav exposing (Key)
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Process
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import GameLogic
    exposing
        ( checkBigBoardWinner
        , checkWinner
        , emptySmallBoard
        , getAllAvailableMoves
        , initialBoard
        , isBigBoardComplete
        , isSmallBoardComplete
        , isValidMove
        , makeMove
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (Language(..), Translation, languageToString, stringToLanguage, translations)
import Json.Decode as D
import Json.Encode as E
import Lamdera
import Lamdera.Json as Json
import List.Extra as List
import LocalStorage exposing (LocalStorageUpdate(..), localStorageDecoder)
import Palette.Button as Button
import Palette.Modal as Modal
import Palette.Layout as Layout
import Palette.Typography as T
import Palette.Container as Container
import Palette.Animation as Animation
import Palette.Utils as Utils
import String
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes
import Theme exposing (..)
import Tutorial.Tutorial exposing (getTutorialBoard, isTutorialMoveValid)
import Tutorial.Types exposing (TutorialStep(..))
import Tutorial.View exposing (viewTutorialCell)
import Types exposing (..)
import Url


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = init
    , onUrlRequest = UrlClicked
    , onUrlChange = UrlChanged
    , update = update
    , updateFromBackend = updateFromBackend
    , subscriptions = subscriptions
    , view = view
    }


init : Url.Url -> Key -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    ( { key = key
      , board = initialBoard X
      , route = Home
      , language = Nothing
      , userPreference = SystemMode
      , systemMode = Light
      , moveHistory = []
      , currentMoveIndex = -1
      , rulesModalVisible = False
      , humanPlaysFirst = True
      , frClickCount = 0
      , debuggerVisible = False
      , debuggerPosition = { x = 0, y = 0 }
      , isDraggingDebugger = False
      , dragOffset = { x = 0, y = 0 }
      , debuggerSize = { width = 300, height = 200 }
      , isResizingDebugger = False
      , localStorage = Nothing
      , selectedDifficulty = Nothing
      , onlinePlayer = Nothing
      , showAbandonConfirm = False
      , gameResult = Ongoing
      , tutorialState = Nothing
      , botDifficultyMenuOpen = False
      , botThinking = False
      , inMatchmaking = False
      , onlineOpponent = Nothing
      , isLoading = True
      , loadingProgress = 0
      }
    , Command.batch
        [ LocalStorage.getLocalStorage
        , startLoadingAnimation
        ]
    )


startLoadingAnimation : Command FrontendOnly ToBackend FrontendMsg
startLoadingAnimation =
    Effect.Process.sleep (Duration.milliseconds 1000)
        |> Effect.Task.perform (\_ -> LoadingComplete)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Command.none )

        CellClicked boardIndex cellIndex ->
            if model.currentMoveIndex < List.length model.moveHistory - 1 then
                ( model, Command.none )

            else
                case model.route of
                    Game (WithBot difficulty) ->
                        if model.board.currentPlayer == O then
                            ( model, Command.none )

                        else
                            let
                                ( updatedModel, cmd ) =
                                    handlePlayerMove model boardIndex cellIndex
                            in
                            if updatedModel.board.winner == Nothing then
                                ( { updatedModel | botThinking = True }
                                , Command.batch [ cmd, Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 500)) ]
                                )

                            else
                                ( updatedModel, cmd )

                    Game WithFriend ->
                        handlePlayerMove model boardIndex cellIndex

                    Game OnlineGame ->
                        model.onlinePlayer
                            |> Maybe.map
                                (\player ->
                                    if player == model.board.currentPlayer then
                                        handlePlayerMove model boardIndex cellIndex

                                    else
                                        ( model, Command.none )
                                )
                            |> Maybe.withDefault ( model, Command.none )

                    Home ->
                        ( model, Command.none )

        BotMove ->
            case model.route of
                Game (WithBot difficulty) ->
                    if model.board.currentPlayer == O then
                        let
                            botMove =
                                Bot.findBestMove model.board difficulty

                            ( newModel, cmd ) =
                                case botMove of
                                    Just ( boardIdx, cellIdx ) ->
                                        let
                                            ( modelAfterMove, moveCmd ) =
                                                handlePlayerMove model boardIdx cellIdx
                                        in
                                        if modelAfterMove.board.winner == Nothing then
                                            ( { modelAfterMove | botThinking = True }
                                            , Command.batch [ moveCmd, Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 500)) ]
                                            )

                                        else
                                            ( modelAfterMove, moveCmd )

                                    Nothing ->
                                        ( model, Command.none )
                        in
                        ( newModel, cmd )

                    else
                        ( model, Command.none )

                Game WithFriend ->
                    ( model, Command.none )

                Game OnlineGame ->
                    ( model, Command.none )

                Home ->
                    ( model, Command.none )

        RestartGame ->
            ( { model
                | board = initialBoard X
                , moveHistory = []
                , currentMoveIndex = -1
              }
            , Command.none
            )

        StartGameWithFriend ->
            ( { model
                | route = Game WithFriend
                , board = initialBoard X
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Ongoing
              }
            , Audio.playButtonClick
            )

        StartGameWithBot ->
            ( { model | botDifficultyMenuOpen = True }
            , Audio.playButtonClick
            )

        SelectBotDifficulty difficulty ->
            ( { model
                | selectedDifficulty = Just difficulty
              }
            , Audio.playButtonClick
            )

        StartWithPlayer humanStarts ->
            let
                startingPlayer =
                    if humanStarts then
                        X

                    else
                        O

                newBoard =
                    initialBoard startingPlayer

                cmd =
                    if not humanStarts then
                        Command.batch
                            [ Audio.playButtonClick
                            , Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 500))
                            ]

                    else
                        Audio.playButtonClick
            in
            case model.route of
                Game (WithBot _) ->
                    ( { model
                        | board = newBoard
                        , moveHistory = []
                        , currentMoveIndex = -1
                        , humanPlaysFirst = humanStarts
                        , botThinking = not humanStarts
                        , gameResult = Ongoing
                      }
                    , cmd
                    )

                _ ->
                    case model.selectedDifficulty of
                        Just difficulty ->
                            ( { model
                                | route = Game (WithBot difficulty)
                                , botDifficultyMenuOpen = False
                                , board = newBoard
                                , moveHistory = []
                                , currentMoveIndex = -1
                                , selectedDifficulty = Nothing
                                , humanPlaysFirst = humanStarts
                                , botThinking = not humanStarts
                                , gameResult = Ongoing
                              }
                            , cmd
                            )

                        Nothing ->
                            ( model, Command.none )

        ReturnToMenu ->
            ( { model
                | route = Home
                , gameResult = Ongoing
              }
            , Audio.playButtonClick
            )

        CancelBotDifficulty ->
            ( { model | botDifficultyMenuOpen = False }
            , Audio.playButtonClick
            )

        PlayForMe ->
            case model.route of
                Game (WithBot _) ->
                    if model.board.currentPlayer == X && model.board.winner == Nothing then
                        let
                            botMove =
                                Bot.findBestMove model.board Elite

                            ( newModel, cmd ) =
                                case botMove of
                                    Just ( boardIdx, cellIdx ) ->
                                        let
                                            ( modelAfterMove, moveCmd ) =
                                                handlePlayerMove model boardIdx cellIdx
                                        in
                                        if modelAfterMove.board.winner == Nothing then
                                            ( { modelAfterMove | botThinking = True }
                                            , Command.batch [ moveCmd, Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 500)) ]
                                            )

                                        else
                                            ( modelAfterMove, moveCmd )

                                    Nothing ->
                                        ( model, Command.none )
                        in
                        ( newModel, cmd )

                    else
                        ( model, Command.none )

                _ ->
                    ( model, Command.none )

        ChangeLanguage lang ->
            let
                newModel =
                    { model
                        | language = Just lang
                        , frClickCount =
                            if lang == FR then
                                model.frClickCount + 1

                            else
                                model.frClickCount
                    }
            in
            ( newModel
            , Command.batch
                [ LocalStorage.storeValue (LanguageUpdate lang)
                , Audio.playButtonClick
                ]
            )

        CloseDebugger ->
            ( { model | debuggerVisible = False, frClickCount = 0 }, Command.none )

        UndoMove ->
            if model.currentMoveIndex >= 0 then
                let
                    newIndex =
                        model.currentMoveIndex - 1

                    newBoard =
                        reconstructBoardFromMoves model.moveHistory newIndex model.board
                in
                ( { model
                    | currentMoveIndex = newIndex
                    , board = newBoard
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        RedoMove ->
            if model.currentMoveIndex < List.length model.moveHistory - 1 then
                let
                    newIndex =
                        model.currentMoveIndex + 1

                    newBoard =
                        reconstructBoardFromMoves model.moveHistory newIndex model.board
                in
                ( { model
                    | currentMoveIndex = newIndex
                    , board = newBoard
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        ToggleDarkMode ->
            let
                newPreference =
                    case model.userPreference of
                        DarkMode ->
                            LightMode

                        LightMode ->
                            SystemMode

                        SystemMode ->
                            DarkMode
            in
            ( { model | userPreference = newPreference }
            , Command.batch
                [ LocalStorage.storeValue (ThemePreferenceUpdate newPreference)
                , Audio.playButtonClick
                ]
            )

        ToggleDebugMode ->
            ( model, Command.none )

        ReceivedLocalStorage { language, userPreference, systemMode } ->
            ( { model
                | language = Just language
                , userPreference = userPreference
                , systemMode = systemMode
              }
            , Command.none
            )

        StartDraggingDebugger mouseX mouseY ->
            ( { model
                | isDraggingDebugger = True
                , dragOffset =
                    { x = mouseX - model.debuggerPosition.x
                    , y = mouseY - model.debuggerPosition.y
                    }
              }
            , Command.none
            )

        StopDraggingDebugger ->
            ( { model | isDraggingDebugger = False }, Command.none )

        DragDebugger mouseX mouseY ->
            if model.isDraggingDebugger then
                ( { model
                    | debuggerPosition =
                        { x = mouseX - model.dragOffset.x
                        , y = mouseY - model.dragOffset.y
                        }
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        StartResizingDebugger ->
            ( { model | isResizingDebugger = True }, Command.none )

        StopResizingDebugger ->
            ( { model | isResizingDebugger = False }, Command.none )

        ResizeDebugger mouseX mouseY ->
            if model.isResizingDebugger then
                let
                    newWidth =
                        mouseX - model.debuggerPosition.x

                    newHeight =
                        mouseY - model.debuggerPosition.y

                    constrainedWidth =
                        Basics.max 200 newWidth

                    constrainedHeight =
                        Basics.max 150 newHeight
                in
                ( { model
                    | debuggerSize =
                        { width = constrainedWidth
                        , height = constrainedHeight
                        }
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        NoOp ->
            ( model, Command.none )

        ToggleRulesModal ->
            ( { model | rulesModalVisible = not model.rulesModalVisible }
            , Audio.playButtonClick
            )

        StartOnlineGame ->
            ( { model | inMatchmaking = True }
            , Command.batch
                [ Effect.Lamdera.sendToBackend JoinMatchmaking
                , Audio.playOnlineSound
                ]
            )

        LeaveMatchmaking ->
            ( { model | inMatchmaking = False }
            , Effect.Lamdera.sendToBackend LeaveMatchmakingToBackend
            )

        StartWithRandomPlayer ->
            ( model, Effect.Task.perform GotTime Effect.Time.now )

        GotTime time ->
            let
                randomValue =
                    modBy 2 (Effect.Time.posixToMillis time)

                humanStarts =
                    randomValue == 0

                startingPlayer =
                    if humanStarts then
                        X

                    else
                        O

                newBoard =
                    initialBoard startingPlayer

                cmd =
                    if not humanStarts then
                        Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 500))

                    else
                        Command.none
            in
            case model.route of
                Game (WithBot _) ->
                    ( { model
                        | board = newBoard
                        , moveHistory = []
                        , currentMoveIndex = -1
                        , humanPlaysFirst = humanStarts
                        , botThinking = not humanStarts
                        , gameResult = Ongoing
                      }
                    , cmd
                    )

                _ ->
                    case model.selectedDifficulty of
                        Just difficulty ->
                            ( { model
                                | route = Game (WithBot difficulty)
                                , botDifficultyMenuOpen = False
                                , board = newBoard
                                , moveHistory = []
                                , currentMoveIndex = -1
                                , selectedDifficulty = Nothing
                                , humanPlaysFirst = humanStarts
                                , botThinking = not humanStarts
                                , gameResult = Ongoing
                              }
                            , cmd
                            )

                        Nothing ->
                            ( model, Command.none )

        Tick newTime ->
            ( model, Command.none )

        ShowAbandonConfirm ->
            ( { model | showAbandonConfirm = True }, Command.none )

        HideAbandonConfirm ->
            ( { model | showAbandonConfirm = False }, Command.none )

        ConfirmAbandon ->
            ( { model
                | showAbandonConfirm = False
                , onlineOpponent = Nothing
                , onlinePlayer = Nothing
                , board = initialBoard X
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Lost
              }
            , Command.batch
                [ Effect.Lamdera.sendToBackend AbandonGame
                , Audio.playLoseSound
                ]
            )

        StartTutorial ->
            ( { model
                | tutorialState = Just TutorialBasicMove
                , board = getTutorialBoard TutorialBasicMove
                , route = Game WithFriend
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Ongoing
                , rulesModalVisible = False
              }
            , Audio.playButtonClick
            )

        NextTutorialStep ->
            case model.tutorialState of
                Just tutorialState ->
                    let
                        nextStep =
                            case tutorialState of
                                TutorialBasicMove ->
                                    Just TutorialBoardSelection

                                TutorialBoardSelection ->
                                    Just TutorialWinningSmall

                                TutorialWinningSmall ->
                                    Just TutorialFreeChoice

                                TutorialFreeChoice ->
                                    Just TutorialWinningBig

                                TutorialWinningBig ->
                                    Nothing
                    in
                    ( { model
                        | tutorialState = nextStep
                        , board =
                            case nextStep of
                                Just step ->
                                    getTutorialBoard step

                                Nothing ->
                                    initialBoard X
                      }
                    , Audio.playButtonClick
                    )

                _ ->
                    ( model, Command.none )

        CompleteTutorial ->
            ( { model
                | tutorialState = Nothing
                , route = Home
                , board = initialBoard X
              }
            , Audio.playButtonClick
            )

        UpdateLoadingProgress progress ->
            ( { model | loadingProgress = progress }, Command.none )

        LoadingComplete ->
            ( { model | isLoading = False }, Command.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly toMsg FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )

        GameFound data ->
            ( { model
                | inMatchmaking = False
                , onlineOpponent = Just data.opponentId
                , route = Game OnlineGame
                , board = initialBoard X
                , onlinePlayer = Just data.playerRole
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Ongoing
              }
            , Command.none
            )

        OpponentMove move ->
            let
                newBoard =
                    makeMove model.board move.boardIndex move.cellIndex move.player

                newHistory =
                    List.take (model.currentMoveIndex + 1) model.moveHistory ++ [ move ]

                newIndex =
                    List.length newHistory - 1

                newGameResult =
                    case newBoard.winner of
                        Just winner ->
                            case model.onlinePlayer of
                                Just myRole ->
                                    if myRole == winner then
                                        Won

                                    else
                                        Lost

                                Nothing ->
                                    Ongoing

                        Nothing ->
                            if isBigBoardComplete newBoard then
                                Drew

                            else
                                Ongoing

                soundCommand =
                    let
                        oldSmallBoard =
                            List.getAt move.boardIndex model.board.boards
                                |> Maybe.withDefault GameLogic.emptySmallBoard

                        newSmallBoard =
                            List.getAt move.boardIndex newBoard.boards
                                |> Maybe.withDefault GameLogic.emptySmallBoard
                    in
                    if newSmallBoard.winner /= Nothing && oldSmallBoard.winner == Nothing then
                        Audio.playSmallWinSound

                    else
                        Audio.playMoveSound move.player
            in
            ( { model
                | board = newBoard
                , moveHistory = newHistory
                , currentMoveIndex = newIndex
                , gameResult = newGameResult
              }
            , soundCommand
            )

        OpponentLeft ->
            ( { model
                | onlineOpponent = Nothing
                , onlinePlayer = Nothing
                , gameResult = Won
              }
            , Audio.playWinSound
            )


handlePlayerMove : FrontendModel -> Int -> Int -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
handlePlayerMove model boardIndex cellIndex =
    let
        canPlayInBoard =
            case model.board.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    boardIndex == activeBoardIndex

        board =
            model.board

        smallBoard =
            List.getAt boardIndex board.boards

        isCellEmpty =
            case smallBoard of
                Just sb ->
                    case List.getAt cellIndex sb.cells of
                        Just cell ->
                            cell == Empty

                        Nothing ->
                            False

                Nothing ->
                    False

        isSmallBoardAvailable =
            case smallBoard of
                Just sb ->
                    sb.winner == Nothing && not (List.all (\cell -> cell /= Empty) sb.cells)

                Nothing ->
                    False

        move =
            { boardIndex = boardIndex
            , cellIndex = cellIndex
            , player = board.currentPlayer
            }

        newBoard =
            makeMove board boardIndex cellIndex board.currentPlayer

        newHistory =
            List.take (model.currentMoveIndex + 1) model.moveHistory ++ [ move ]

        newIndex =
            List.length newHistory - 1

        shouldProgressTutorial =
            case model.tutorialState of
                Just step ->
                    case step of
                        TutorialWinningSmall ->
                            canPlayInBoard
                                && isCellEmpty
                                && isSmallBoardAvailable
                                && boardIndex
                                == 4
                                && cellIndex
                                == 4
                                && (let
                                        boardAfterMove =
                                            makeMove board boardIndex cellIndex board.currentPlayer

                                        centerBoard =
                                            List.getAt 4 boardAfterMove.boards
                                    in
                                    case centerBoard of
                                        Just sb ->
                                            sb.winner == Just X

                                        Nothing ->
                                            False
                                   )

                        TutorialWinningBig ->
                            canPlayInBoard
                                && isCellEmpty
                                && isSmallBoardAvailable
                                && boardIndex
                                == 8
                                && cellIndex
                                == 6
                                && (let
                                        boardAfterMove =
                                            makeMove board boardIndex cellIndex board.currentPlayer
                                    in
                                    boardAfterMove.winner == Just X
                                   )

                        _ ->
                            canPlayInBoard && isCellEmpty && isSmallBoardAvailable && isTutorialMoveValid step boardIndex cellIndex newBoard

                _ ->
                    False

        nextTutorialStep =
            if shouldProgressTutorial then
                case model.tutorialState of
                    Just TutorialBasicMove ->
                        Just TutorialBoardSelection

                    Just TutorialBoardSelection ->
                        Just TutorialWinningSmall

                    Just TutorialWinningSmall ->
                        Just TutorialFreeChoice

                    Just TutorialFreeChoice ->
                        Just TutorialWinningBig

                    Just TutorialWinningBig ->
                        Nothing

                    _ ->
                        Nothing

            else
                Nothing

        newGameResult =
            case model.route of
                Game (WithBot _) ->
                    case newBoard.winner of
                        Just winner ->
                            if winner == X then
                                Won

                            else
                                Lost

                        Nothing ->
                            if isBigBoardComplete newBoard then
                                Drew

                            else
                                Ongoing

                Game OnlineGame ->
                    case newBoard.winner of
                        Just winner ->
                            case model.onlinePlayer of
                                Just myRole ->
                                    if myRole == winner then
                                        Won

                                    else
                                        Lost

                                Nothing ->
                                    Ongoing

                        Nothing ->
                            if isBigBoardComplete newBoard then
                                Drew

                            else
                                Ongoing

                _ ->
                    case model.tutorialState of
                        Just TutorialWinningBig ->
                            if shouldProgressTutorial then
                                Won

                            else
                                model.gameResult

                        _ ->
                            model.gameResult

        updatedModel =
            { model
                | board = newBoard
                , moveHistory = newHistory
                , currentMoveIndex = newIndex
                , tutorialState =
                    case ( model.tutorialState, nextTutorialStep ) of
                        ( Just TutorialWinningBig, Nothing ) ->
                            if shouldProgressTutorial then
                                Nothing

                            else
                                model.tutorialState

                        ( _, Just step ) ->
                            Just step

                        _ ->
                            model.tutorialState
                , gameResult = newGameResult
            }

        soundCommand =
            if canPlayInBoard && isCellEmpty && isSmallBoardAvailable then
                case newGameResult of
                    Won ->
                        Audio.playWinSound

                    Lost ->
                        Audio.playLoseSound

                    Drew ->
                        Audio.playDrawSound

                    Ongoing ->
                        let
                            newSmallBoard =
                                List.getAt boardIndex newBoard.boards
                                    |> Maybe.withDefault GameLogic.emptySmallBoard

                            oldSmallBoard =
                                List.getAt boardIndex board.boards
                                    |> Maybe.withDefault GameLogic.emptySmallBoard
                        in
                        if newSmallBoard.winner /= Nothing && oldSmallBoard.winner == Nothing then
                            Audio.playSmallWinSound

                        else
                            Audio.playMoveSound board.currentPlayer

            else
                Audio.playErrorSound
    in
    if canPlayInBoard && isCellEmpty && isSmallBoardAvailable then
        case model.route of
            Game (WithBot difficulty) ->
                if model.board.currentPlayer == X then
                    ( updatedModel
                    , Command.batch
                        [ soundCommand
                        , Effect.Process.sleep (Duration.milliseconds 500)
                            |> Effect.Task.perform (\_ -> BotMove)
                        ]
                    )

                else
                    ( updatedModel, soundCommand )

            Game OnlineGame ->
                case model.onlinePlayer of
                    Just player ->
                        if player == model.board.currentPlayer then
                            ( updatedModel
                            , Command.batch
                                [ soundCommand
                                , Effect.Lamdera.sendToBackend (MakeMove boardIndex cellIndex player)
                                ]
                            )

                        else
                            ( model, Command.none )

                    Nothing ->
                        ( model, Command.none )

            _ ->
                ( updatedModel, soundCommand )

    else
        ( model, Audio.playErrorSound )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    case model.language of
        Nothing ->
            { title = "Tic-Tac-Toe", body = [ viewLoadingScreen { t = translations EN, c = themes model.userPreference model.systemMode } model ] }

        Just language ->
            let
                ({ c, t } as userConfig) =
                    { t = translations language
                    , c = themes model.userPreference model.systemMode
                    }
            in
            { title = t.welcome
            , body =
                [ Html.node "link"
                    [ attribute "rel" "stylesheet"
                    , attribute "href" "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap"
                    ]
                    []
                , Html.node "style"
                    []
                    [ text """
                        *:not(.game-symbol) {
                            font-family: 'Press Start 2P', cursive !important;
                        }
                        @keyframes thinking {
                            0%, 100% { opacity: 0.3; transform: scale(0.8); }
                            50% { opacity: 1; transform: scale(1.2); }
                        }
                        @keyframes blink {
                            0% { opacity: 0.5; }
                            50% { opacity: 1; }
                            100% { opacity: 0.5; }
                        }
                        @keyframes bigBoardBlink {
                            0% { opacity: 0.8; }
                            50% { opacity: 1; }
                            100% { opacity: 0.8; }
                        }
                        @keyframes slideIn {
                            0% { transform: translateY(-20px); opacity: 0; }
                            100% { transform: translateY(0); opacity: 1; }
                        }
                        @keyframes pulse {
                            0% { transform: scale(1); }
                            50% { transform: scale(1.05); }
                            100% { transform: scale(1); }
                        }
                    """
                    ]
                , div
                    [ style "position" "relative"
                    , style "min-height" "100vh"
                    , style "width" "100%"
                    , style "background" c.gradientBackground
                    ]
                    [ div
                        [ style "min-height" "100vh"
                        , style "background" c.gradientBackground
                        , style "min-height" "100dvh"
                        , style "width" "100%"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        , style "padding" "env(safe-area-inset-top, 10px) env(safe-area-inset-right, 10px) env(safe-area-inset-bottom, 10px) env(safe-area-inset-left, 10px)"
                        , style "box-sizing" "border-box"
                        , style "position" "absolute"
                        , style "top" "0"
                        , style "left" "0"
                        , style "letter-spacing" "1px"
                        , style "line-height" "1.5"
                        , style "opacity"
                            (if model.isLoading then
                                "0"

                             else
                                "1"
                            )
                        , style "transition" "opacity 0.3s ease-in"
                        ]
                        ([ viewLanguageSelector userConfig model
                         , case model.route of
                            Home ->
                                viewHome userConfig model

                            Game mode ->
                                viewGame userConfig model mode
                         ]
                            ++ Debugger.view userConfig model
                            ++ [ if model.gameResult /= Ongoing then
                                    viewGameResultModal userConfig model

                                 else
                                    text ""
                               , if model.rulesModalVisible then
                                    viewRulesModal userConfig model

                                 else
                                    text ""
                               , if model.tutorialState /= Nothing then
                                    viewTutorialOverlay userConfig model

                                 else
                                    text ""
                               ]
                        )
                    , if model.isLoading then
                        viewLoadingScreen userConfig model

                      else
                        text ""
                    ]
                ]
            }


viewGameButton : UserConfig -> HtmlId -> String -> FrontendMsg -> Html FrontendMsg
viewGameButton userConfig htmlId label msg =
    button
        (Button.secondary userConfig ++
            Utils.margin10 ++
            Utils.fullWidth ++
            Utils.maxWidth 300 ++
            [ onClick msg
            , Dom.idToAttribute htmlId
            ]
        )
        [ text label ]


viewHome : UserConfig -> FrontendModel -> Html FrontendMsg
viewHome ({ t, c } as userConfig) model =
    div
        (Container.card c ++
            [ style "text-align" "center"
            , style "max-width" "400px"
            , style "width" "90%"
            , style "color" c.text
            ]
        )
        [ if model.botDifficultyMenuOpen then
            viewBotDifficultyMenu userConfig model

          else
            div []
                [ T.h1 c t.welcome
                , T.text c t.description
                , div
                    Layout.flexColumnCenter
                    [ viewGameButton userConfig (Dom.id "gameWithFriend") t.playWithFriend StartGameWithFriend
                    , viewGameButton userConfig (Dom.id "gameWithBot") t.playWithBot StartGameWithBot
                    , viewGameButton userConfig (Dom.id "toggleRules") t.rulesTitle ToggleRulesModal
                    , button
                        (Button.secondary userConfig ++
                            Button.withShadow ++
                            (if model.inMatchmaking then
                                Button.disabled
                             else
                                []
                            ) ++
                            Utils.margin10 ++
                            Utils.fullWidth ++
                            Utils.maxWidth 300 ++
                            [ onClick
                                (if model.inMatchmaking then
                                    LeaveMatchmaking
                                 else
                                    StartOnlineGame
                                )
                            , Dom.idToAttribute <|
                                Dom.id
                                    (if model.inMatchmaking then
                                        "leave-matchmaking-button"
                                     else
                                        "start-online-game-button"
                                    )
                            ]
                        )
                        [ if model.inMatchmaking then
                            div
                                Layout.flexCenterWithGap
                                [ text t.searching
                                , viewThinkingIndicator
                                ]
                          else
                            text t.playOnline
                        ]
                    ]
                ]
        , if model.rulesModalVisible then
            viewRulesModal userConfig model

          else
            text ""
        ]


viewGame : UserConfig -> FrontendModel -> GameMode -> Html FrontendMsg
viewGame ({ t, c } as userConfig) model mode =
    let
        gameTitle =
            case model.tutorialState of
                Just _ ->
                    t.tutorialTitle

                Nothing ->
                    case mode of
                        WithFriend ->
                            t.playingWithFriend

                        WithBot difficulty ->
                            t.playingWithBot
                                (case difficulty of
                                    Easy ->
                                        t.easy

                                    Medium ->
                                        t.medium

                                    Hard ->
                                        t.hard

                                    Elite ->
                                        t.elite
                                )

                        OnlineGame ->
                            t.playingOnline
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "padding-top" "20px"
        , style "padding-bottom" "20px"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "width" "100%"
        , style "max-width" "100vh"
        , style "margin" "auto"
        , style "box-sizing" "border-box"
        , style "position" "relative"
        , style "height" "100%"
        , style "background-color" c.background
        , style "color" c.text
        ]
        [ div
            [ style "text-align" "center"
            , style "padding" "10px"
            , style "flex-shrink" "0"
            ]
            [ text gameTitle
            , viewStatus userConfig model
            ]
        , div
            [ style "flex" "1"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "min-height" "0"
            , style "padding" "10px"
            , style "position" "relative"
            ]
            [ div
                [ style "width" "min(100%, calc(100vh - 300px))"
                , style "aspect-ratio" "1/1"
                ]
                [ viewBigBoard userConfig model ]
            ]
        , div
            [ style "padding" "10px"
            , style "flex-shrink" "0"
            ]
            [ case mode of
                WithBot _ ->
                    div []
                        [ button
                            [ style "padding" "12px"
                            , style "font-size" "0.8em"
                            , style "font-family" "inherit"
                            , style "background-color"
                                (if model.board.currentPlayer == X && model.board.winner == Nothing && not model.botThinking then
                                    Color.primary

                                 else
                                    Color.disabled
                                )
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "10px"
                            , style "cursor"
                                (if model.board.currentPlayer == X && model.board.winner == Nothing && not model.botThinking then
                                    "pointer"

                                 else
                                    "not-allowed"
                                )
                            , style "transition" "all 0.2s ease"
                            , style "margin-bottom" "10px"
                            , style "width" "100%"
                            , onClick PlayForMe
                            ]
                            [ text t.playForMe ]
                        , if model.showAbandonConfirm then
                            div
                                [ style "display" "flex"
                                , style "gap" "10px"
                                , style "margin-bottom" "10px"
                                ]
                                [ button
                                    [ style "flex" "1"
                                    , style "padding" "12px"
                                    , style "font-size" "0.8em"
                                    , style "font-family" "inherit"
                                    , style "background-color" Color.danger
                                    , style "color" "white"
                                    , style "border" "none"
                                    , style "border-radius" "10px"
                                    , style "cursor" "pointer"
                                    , style "transition" "all 0.2s ease"
                                    , onClick ConfirmAbandon
                                    ]
                                    [ text t.confirmAbandon ]
                                , button
                                    [ style "flex" "1"
                                    , style "padding" "12px"
                                    , style "font-size" "0.8em"
                                    , style "font-family" "inherit"
                                    , style "background-color" Color.primary
                                    , style "color" "white"
                                    , style "border" "none"
                                    , style "border-radius" "10px"
                                    , style "cursor" "pointer"
                                    , style "transition" "all 0.2s ease"
                                    , onClick HideAbandonConfirm
                                    ]
                                    [ text t.cancelAbandon ]
                                ]

                          else
                            button
                                [ style "padding" "12px"
                                , style "font-size" "0.8em"
                                , style "font-family" "inherit"
                                , style "background-color" Color.danger
                                , style "color" "white"
                                , style "border" "none"
                                , style "border-radius" "10px"
                                , style "cursor" "pointer"
                                , style "transition" "all 0.2s ease"
                                , style "margin-bottom" "10px"
                                , style "width" "100%"
                                , onClick ShowAbandonConfirm
                                ]
                                [ text t.abandon ]
                        ]

                OnlineGame ->
                    if model.onlineOpponent /= Nothing then
                        if model.showAbandonConfirm then
                            div
                                [ style "display" "flex"
                                , style "gap" "10px"
                                , style "margin-bottom" "10px"
                                ]
                                [ button
                                    [ style "flex" "1"
                                    , style "padding" "12px"
                                    , style "font-size" "0.8em"
                                    , style "font-family" "inherit"
                                    , style "background-color" Color.danger
                                    , style "color" "white"
                                    , style "border" "none"
                                    , style "border-radius" "10px"
                                    , style "cursor" "pointer"
                                    , style "transition" "all 0.2s ease"
                                    , onClick ConfirmAbandon
                                    ]
                                    [ text t.confirmAbandon ]
                                , button
                                    [ style "flex" "1"
                                    , style "padding" "12px"
                                    , style "font-size" "0.8em"
                                    , style "font-family" "inherit"
                                    , style "background-color" Color.primary
                                    , style "color" "white"
                                    , style "border" "none"
                                    , style "border-radius" "10px"
                                    , style "cursor" "pointer"
                                    , style "transition" "all 0.2s ease"
                                    , onClick HideAbandonConfirm
                                    ]
                                    [ text t.cancelAbandon ]
                                ]

                        else
                            button
                                [ style "padding" "12px"
                                , style "font-size" "0.8em"
                                , style "font-family" "inherit"
                                , style "background-color" Color.danger
                                , style "color" "white"
                                , style "border" "none"
                                , style "border-radius" "10px"
                                , style "cursor" "pointer"
                                , style "transition" "all 0.2s ease"
                                , style "margin-bottom" "10px"
                                , style "width" "100%"
                                , onClick ShowAbandonConfirm
                                ]
                                [ text t.abandon ]

                    else
                        text ""

                WithFriend ->
                    if model.tutorialState == Nothing then
                        button
                            [ style "padding" "12px"
                            , style "font-size" "0.8em"
                            , style "font-family" "inherit"
                            , style "background-color" Color.danger
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "10px"
                            , style "cursor" "pointer"
                            , style "transition" "all 0.2s ease"
                            , style "margin-bottom" "10px"
                            , style "width" "100%"
                            , onClick ReturnToMenu
                            , Dom.idToAttribute (Dom.id "back-to-menu-button")
                            ]
                            [ text t.backToMenu ]

                    else
                        text ""
            , if model.tutorialState == Nothing then
                div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    ]
                    [ button
                        [ style "flex" "1"
                        , style "padding" "8px"
                        , style "font-size" "1.2em"
                        , style "background-color"
                            (if model.currentMoveIndex >= 0 then
                                Color.primary

                             else
                                Color.disabled
                            )
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor"
                            (if model.currentMoveIndex >= 0 then
                                "pointer"

                             else
                                "not-allowed"
                            )
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        , onClick UndoMove
                        ]
                        [ text "↩" ]
                    , button
                        [ style "flex" "1"
                        , style "padding" "8px"
                        , style "font-size" "1.2em"
                        , style "background-color"
                            (if model.currentMoveIndex < List.length model.moveHistory - 1 then
                                Color.primary

                             else
                                Color.disabled
                            )
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor"
                            (if model.currentMoveIndex < List.length model.moveHistory - 1 then
                                "pointer"

                             else
                                "not-allowed"
                            )
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "center"
                        , onClick RedoMove
                        ]
                        [ text "↪" ]
                    ]

              else
                text ""
            ]
        ]


viewBotDifficultyMenu : UserConfig -> FrontendModel -> Html FrontendMsg
viewBotDifficultyMenu ({ t, c } as userConfig) model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        , style "width" "100%"
        ]
        [ h2
            [ style "margin" "0 0 15px 0"
            , style "color" c.text
            , style "font-size" "1.5em"
            ]
            [ text t.chooseDifficulty ]
        , viewDifficultyButton userConfig model t.easy Easy
        , viewDifficultyButton userConfig model t.medium Medium
        , viewDifficultyButton userConfig model t.hard Hard
        , viewDifficultyButton userConfig model t.elite Elite
        , case model.selectedDifficulty of
            Just _ ->
                div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    , style "margin-top" "10px"
                    ]
                    [ button
                        [ style "flex" "1"
                        , style "padding" "8px"
                        , style "font-size" "0.9em"
                        , style "background-color" Color.primary
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor" "pointer"
                        , style "font-family" "inherit"
                        , onClick (StartWithPlayer True)
                        , Dom.idToAttribute (Dom.id "human-starts-button")
                        ]
                        [ text t.humanStarts ]
                    , button
                        [ style "flex" "1"
                        , style "padding" "8px"
                        , style "font-size" "0.9em"
                        , style "background-color" Color.primary
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor" "pointer"
                        , style "font-family" "inherit"
                        , onClick (StartWithPlayer False)
                        , Dom.idToAttribute (Dom.id "bot-starts-button")
                        ]
                        [ text t.botStarts ]
                    , button
                        [ style "flex" "1"
                        , style "padding" "8px"
                        , style "font-size" "0.9em"
                        , style "background-color" Color.primary
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor" "pointer"
                        , style "font-family" "inherit"
                        , onClick StartWithRandomPlayer
                        , Dom.idToAttribute (Dom.id "random-starts-button")
                        ]
                        [ text t.randomStarts ]
                    ]

            Nothing ->
                text ""
        , button
            [ style "padding" "12px"
            , style "font-size" "1.1em"
            , style "font-weight" "600"
            , style "background-color" Color.danger
            , style "color" "white"
            , style "border" "none"
            , style "border-radius" "8px"
            , style "cursor" "pointer"
            , style "transition" "all 0.2s ease"
            , style "width" "100%"
            , style "margin-top" "10px"
            , onClick CancelBotDifficulty
            ]
            [ text t.back ]
        ]


viewDifficultyButton : UserConfig -> FrontendModel -> String -> BotDifficulty -> Html FrontendMsg
viewDifficultyButton ({ t, c } as userConfig) model label difficulty =
    button
        [ style "padding" "12px"
        , style "font-size" "0.8em"
        , style "font-family" "inherit"
        , style "background-color"
            (if model.selectedDifficulty == Just difficulty then
                Color.success

             else
                c.secondaryBackground
            )
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "8px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "width" "100%"
        , onClick (SelectBotDifficulty difficulty)
        ]
        [ text label ]


playerToString : Translation -> Player -> String
playerToString t player =
    case player of
        X ->
            t.playerX

        O ->
            t.playerO


viewStatus : UserConfig -> FrontendModel -> Html msg
viewStatus ({ t, c } as userConfig) model =
    div
        [ style "margin" "10px 0"
        , style "color" c.text
        , style "font-size" "0.7em"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "gap" "10px"
            ]
            [ text <|
                case model.gameResult of
                    Won ->
                        t.youWon

                    Lost ->
                        t.youLost

                    Drew ->
                        t.draw

                    Ongoing ->
                        case model.board.winner of
                            Just player ->
                                case model.route of
                                    Game OnlineGame ->
                                        case model.onlinePlayer of
                                            Just myRole ->
                                                if myRole == player then
                                                    t.youWon

                                                else
                                                    t.youLost

                                            Nothing ->
                                                case player of
                                                    X ->
                                                        t.playerXWins

                                                    O ->
                                                        t.playerOWins

                                    _ ->
                                        case player of
                                            X ->
                                                t.playerXWins

                                            O ->
                                                t.playerOWins

                            Nothing ->
                                if isBigBoardComplete model.board then
                                    t.draw

                                else
                                    case model.route of
                                        Game (WithBot _) ->
                                            if model.board.currentPlayer == O then
                                                "🤖"

                                            else
                                                t.yourTurn

                                        Game OnlineGame ->
                                            case model.onlinePlayer of
                                                Just player ->
                                                    if model.onlineOpponent == Nothing then
                                                        t.waitingForOpponent

                                                    else if player == model.board.currentPlayer then
                                                        t.yourTurn

                                                    else
                                                        t.enemyTurn

                                                Nothing ->
                                                    t.waitingForOpponent

                                        _ ->
                                            if model.board.currentPlayer == X then
                                                t.playerXTurn

                                            else
                                                t.playerOTurn
            , if model.botThinking then
                viewThinkingIndicator

              else
                text ""
            ]
        ]


viewThinkingIndicator : Html msg
viewThinkingIndicator =
    div
        [ style "display" "inline-flex"
        , style "align-items" "center"
        , style "gap" "4px"
        ]
        [ div [ style "animation" "thinking 1s infinite" ] [ text "." ]
        , div [ style "animation" "thinking 1s infinite 0.3s" ] [ text "." ]
        , div [ style "animation" "thinking 1s infinite 0.6s" ] [ text "." ]
        ]


viewLanguageSelector : UserConfig -> FrontendModel -> Html FrontendMsg
viewLanguageSelector ({ t, c } as userConfig) model =
    div
        [ style "position" "absolute"
        , style "top" "5px"
        , style "right" "20px"
        , style "display" "flex"
        , style "gap" "10px"
        , style "align-items" "center"
        , style "background-color" (Color.withAlpha c.background 0.8)
        , style "padding" "6px"
        , style "border-radius" "10px"
        , style "backdrop-filter" "blur(10px)"
        , style "box-shadow" "0 2px 10px rgba(0, 0, 0, 0.1)"
        , style "z-index" "1000"
        ]
        [ viewDarkModeButton userConfig model
        , div [ style "width" "1px", style "height" "20px", style "background-color" c.border ] []
        , viewLanguageButton "FR" FR (model.language == Just FR) (isDark model.userPreference model.systemMode)
        , viewLanguageButton "EN" EN (model.language == Just EN) (isDark model.userPreference model.systemMode)
        ]


viewDarkModeButton : UserConfig -> FrontendModel -> Html FrontendMsg
viewDarkModeButton ({ t, c } as userConfig) model =
    button
        [ style "padding" "8px"
        , style "font-size" "1.1em"
        , style "background" "none"
        , style "border" "none"
        , style "cursor" "pointer"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-family" "inherit"
        , style "color" c.textHover
        , style "transition" "all 0.2s ease"
        , onClick ToggleDarkMode
        ]
        [ text
            (case model.userPreference of
                DarkMode ->
                    "🌙"

                LightMode ->
                    "☀️"

                SystemMode ->
                    "🌓"
            )
        ]


viewLanguageButton : String -> Language -> Bool -> Bool -> Html FrontendMsg
viewLanguageButton label lang isActive isDark_ =
    button
        [ style "padding" "8px 12px"
        , style "font-size" "0.7em"
        , style "font-family" "inherit"
        , style "background-color"
            (if isActive then
                Color.primary

             else if isDark_ then
                Color.darkSecondaryBackground

             else
                Color.lightBorder
            )
        , style "color"
            (if isActive then
                "white"

             else if isDark_ then
                Color.darkText

             else
                Color.lightText
            )
        , style "border" "none"
        , style "border-radius" "6px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , onClick (ChangeLanguage lang)
        ]
        [ text label ]


refactoredVersionOfViewLanguageSelector : UserConfig -> Language -> Language -> Html FrontendMsg
refactoredVersionOfViewLanguageSelector { c } selectedLang lang =
    button
        [ style "padding" "8px 12px"
        , style "font-size" "0.7em"
        , style "font-family" "inherit"
        , style "background-color"
            (if selectedLang == lang then
                Color.primary

             else
                c.secondaryBackground
            )
        , style "color" c.text
        , style "border" "none"
        , style "border-radius" "6px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , onClick (ChangeLanguage lang)
        ]
        [ Just lang |> languageToString |> String.toUpper |> text ]


viewBigBoard : UserConfig -> FrontendModel -> Html FrontendMsg
viewBigBoard ({ t, c } as userConfig) model =
    let
        isBotTurn =
            case model.route of
                Game (WithBot _) ->
                    model.board.currentPlayer == O && model.botThinking

                _ ->
                    False

        isOpponentTurn =
            case model.route of
                Game OnlineGame ->
                    case model.onlinePlayer of
                        Just player ->
                            player /= model.board.currentPlayer

                        Nothing ->
                            False

                _ ->
                    False

        isViewingHistory =
            model.currentMoveIndex < List.length model.moveHistory - 1

        shouldBlink =
            not isViewingHistory
                && ((isBotTurn && model.board.activeBoard == Nothing)
                        || (isOpponentTurn && model.board.activeBoard == Nothing)
                   )

        board =
            model.board

        boards =
            board.boards

        boardStyle =
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(3, 1fr)"
            , style "gap" "4px"
            , style "width" "100%"
            , style "aspect-ratio" "1/1"
            , style "margin" "0 auto"
            , style "padding" "4px"
            , style "background-color" c.border
            , style "border-radius" "12px"
            ]

        boardElements =
            List.indexedMap (viewSmallBoard userConfig model) boards

        blinkStyle =
            if shouldBlink then
                [ style "animation" "bigBoardBlink 1s ease-in-out infinite" ]

            else
                []
    in
    div
        (boardStyle ++ blinkStyle)
        boardElements


viewSmallBoard : UserConfig -> FrontendModel -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard ({ t, c } as userConfig) model boardIndex smallBoardData =
    let
        isActive =
            case model.board.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    boardIndex == activeBoardIndex

        isBotTurn =
            case model.route of
                Game (WithBot _) ->
                    model.board.currentPlayer == O && model.botThinking

                _ ->
                    False

        isOpponentTurn =
            case model.route of
                Game OnlineGame ->
                    case model.onlinePlayer of
                        Just player ->
                            player /= model.board.currentPlayer

                        Nothing ->
                            False

                _ ->
                    False

        isViewingHistory =
            model.currentMoveIndex < List.length model.moveHistory - 1

        isTutorialBoard =
            case model.tutorialState of
                Just TutorialBasicMove ->
                    boardIndex == 4

                Just TutorialBoardSelection ->
                    True

                Just TutorialWinningSmall ->
                    boardIndex == 4

                Just TutorialWinningBig ->
                    boardIndex == 8

                _ ->
                    True

        isClickable =
            isActive
                && not isBotTurn
                && not isOpponentTurn
                && not isViewingHistory
                && (case model.tutorialState of
                        Just TutorialBoardSelection ->
                            False

                        Just TutorialWinningSmall ->
                            boardIndex == 4

                        Just TutorialFreeChoice ->
                            False

                        _ ->
                            isTutorialBoard
                   )

        borderColor =
            case model.tutorialState of
                Just TutorialBoardSelection ->
                    if boardIndex == 2 then
                        Color.success

                    else
                        c.border

                _ ->
                    if isActive then
                        Color.success

                    else
                        c.border

        backgroundColor =
            case smallBoardData.winner of
                Just X ->
                    Color.playerX

                Just O ->
                    Color.playerO

                Nothing ->
                    if not isTutorialBoard then
                        Color.disabled

                    else
                        c.background

        cellStyle =
            [ style "box-shadow" ("inset 0 0 0 1px " ++ c.border) ]

        shouldBlink =
            not isViewingHistory
                && ((isActive && isBotTurn && model.board.activeBoard /= Nothing)
                        || (isActive && isOpponentTurn)
                   )

        cellElements =
            List.indexedMap (viewCell userConfig model boardIndex isClickable cellStyle) smallBoardData.cells

        blinkStyle =
            if shouldBlink then
                [ style "animation" "blink 1s ease-in-out infinite" ]

            else
                []

        opacity =
            case model.tutorialState of
                Just TutorialWinningSmall ->
                    if boardIndex /= 4 then
                        [ style "opacity" "0.5" ]

                    else
                        []

                _ ->
                    if not isTutorialBoard then
                        [ style "opacity" "0.5" ]

                    else
                        []
    in
    div
        ([ style "background-color" backgroundColor
         , style "border-radius" "8px"
         , style "display" "grid"
         , style "grid-template-columns" "repeat(3, 1fr)"
         , style "gap" "0"
         , style "padding" "4px"
         , style "aspect-ratio" "1/1"
         , style "box-shadow" ("0 0 0 2px " ++ borderColor)
         ]
            ++ blinkStyle
            ++ opacity
        )
        cellElements


viewCell : UserConfig -> FrontendModel -> Int -> Bool -> List (Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewCell ({ t, c } as userConfig) model boardIndex isClickable cellStyles cellIndex cellState =
    case model.tutorialState of
        Just _ ->
            Tutorial.View.viewTutorialCell userConfig
                model
                boardIndex
                (if isClickable then
                    1

                 else
                    0
                )
                cellStyles
                cellIndex
                cellState

        Nothing ->
            let
                ( symbol, textColor ) =
                    case cellState of
                        Empty ->
                            ( Html.text ""
                            , c.text
                            )

                        Filled X ->
                            ( div [ style "width" "100%", style "height" "100%", style "position" "relative" ]
                                [ svg
                                    [ Svg.Attributes.viewBox "0 0 100 100"
                                    , Svg.Attributes.width "100%"
                                    , Svg.Attributes.height "100%"
                                    , Svg.Attributes.fill "none"
                                    , Svg.Attributes.stroke Color.danger
                                    , Svg.Attributes.strokeWidth "10"
                                    , Svg.Attributes.strokeLinecap "round"
                                    , style "position" "absolute"
                                    , style "top" "0"
                                    , style "left" "0"
                                    ]
                                    [ line
                                        [ Svg.Attributes.x1 "20"
                                        , Svg.Attributes.y1 "20"
                                        , Svg.Attributes.x2 "80"
                                        , Svg.Attributes.y2 "80"
                                        ]
                                        []
                                    , line
                                        [ Svg.Attributes.x1 "80"
                                        , Svg.Attributes.y1 "20"
                                        , Svg.Attributes.x2 "20"
                                        , Svg.Attributes.y2 "80"
                                        ]
                                        []
                                    ]
                                ]
                            , Color.danger
                            )

                        Filled O ->
                            ( div [ style "width" "100%", style "height" "100%", style "position" "relative" ]
                                [ svg
                                    [ Svg.Attributes.viewBox "0 0 100 100"
                                    , Svg.Attributes.width "100%"
                                    , Svg.Attributes.height "100%"
                                    , Svg.Attributes.fill "none"
                                    , Svg.Attributes.stroke Color.primary
                                    , Svg.Attributes.strokeWidth "10"
                                    , Svg.Attributes.strokeLinecap "round"
                                    , style "position" "absolute"
                                    , style "top" "0"
                                    , style "left" "0"
                                    ]
                                    [ circle
                                        [ Svg.Attributes.cx "50"
                                        , Svg.Attributes.cy "50"
                                        , Svg.Attributes.r "35"
                                        ]
                                        []
                                    ]
                                ]
                            , Color.primary
                            )

                isCellClickable =
                    isClickable && cellState == Empty

                cursor =
                    if isCellClickable then
                        "pointer"

                    else
                        "default"

                cornerRadius =
                    case cellIndex of
                        0 ->
                            [ style "border-top-left-radius" "4px" ]

                        2 ->
                            [ style "border-top-right-radius" "4px" ]

                        6 ->
                            [ style "border-bottom-left-radius" "4px" ]

                        8 ->
                            [ style "border-bottom-right-radius" "4px" ]

                        _ ->
                            []
            in
            div
                ([ style "width" "100%"
                 , style "aspect-ratio" "1/1"
                 , style "background-color" c.background
                 , style "display" "flex"
                 , style "align-items" "center"
                 , style "justify-content" "center"
                 , style "cursor" cursor
                 , style "color" textColor
                 , style "user-select" "none"
                 , style "position" "relative"
                 , Dom.idToAttribute (cellId boardIndex cellIndex)
                 ]
                    ++ cornerRadius
                    ++ cellStyles
                    ++ (if isCellClickable then
                            [ onClick (CellClicked boardIndex cellIndex) ]

                        else
                            []
                       )
                )
                [ symbol ]


cellId : Int -> Int -> HtmlId
cellId boardIndex cellIndex =
    "cell_" ++ String.fromInt boardIndex ++ "_" ++ String.fromInt cellIndex |> Dom.id


subscriptions : FrontendModel -> Subscription FrontendOnly FrontendMsg
subscriptions model =
    Subscription.batch
        [ LocalStorage.receiveLocalStorage ReceivedLocalStorage
        , if model.isDraggingDebugger then
            Subscription.batch
                [ Effect.Browser.Events.onMouseMove
                    (D.map2 DragDebugger
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                , Effect.Browser.Events.onMouseUp
                    (D.succeed StopDraggingDebugger)
                ]

          else if model.isResizingDebugger then
            Subscription.batch
                [ Effect.Browser.Events.onMouseMove
                    (D.map2 ResizeDebugger
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                , Effect.Browser.Events.onMouseUp
                    (D.succeed StopResizingDebugger)
                ]

          else
            Subscription.none
        ]


reconstructBoardFromMoves : List Move -> Int -> BigBoard -> BigBoard
reconstructBoardFromMoves moves upToIndex initialBoardState =
    let
        movesToApply =
            List.take (upToIndex + 1) moves

        freshBoard =
            { boards = List.repeat 9 GameLogic.emptySmallBoard
            , currentPlayer = initialBoardState.initialPlayer
            , activeBoard = Nothing
            , winner = Nothing
            , initialPlayer = initialBoardState.initialPlayer
            }
    in
    List.foldl
        (\move board ->
            GameLogic.makeMove board move.boardIndex move.cellIndex board.currentPlayer
        )
        freshBoard
        movesToApply


viewRulesModal : UserConfig -> FrontendModel -> Html FrontendMsg
viewRulesModal ({ t, c } as userConfig) model =
    div
        (Modal.overlay ++
            [ onClick ToggleRulesModal ]
        )
        [ div
            (Modal.container c ++
                [ Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True )) ]
            )
            [ T.h2 c t.rulesTitle
            , T.pre c t.rulesText
            , div
                Layout.flexCenterWithGap
                [ button
                    (Button.primary userConfig ++
                        [ onClick StartTutorial ]
                    )
                    [ text t.startTutorial ]
                , button
                    (Button.primary userConfig ++
                        [ onClick ToggleRulesModal ]
                    )
                    [ text t.close ]
                ]
            ]
        ]


shouldEnableNextButton : FrontendModel -> Bool
shouldEnableNextButton model =
    case model.tutorialState of
        Just step ->
            case step of
                TutorialBasicMove ->
                    let
                        centerBoard =
                            List.getAt 4 model.board.boards
                                |> Maybe.withDefault GameLogic.emptySmallBoard
                    in
                    List.getAt 2 centerBoard.cells == Just (Filled X)

                TutorialBoardSelection ->
                    True

                TutorialWinningSmall ->
                    let
                        centerBoard =
                            List.getAt 4 model.board.boards
                                |> Maybe.withDefault GameLogic.emptySmallBoard
                    in
                    centerBoard.winner == Just X

                TutorialFreeChoice ->
                    True

                TutorialWinningBig ->
                    model.board.winner == Just X

        Nothing ->
            False


viewTutorialOverlay : UserConfig -> FrontendModel -> Html FrontendMsg
viewTutorialOverlay ({ t, c } as userConfig) model =
    case model.tutorialState of
        Just step ->
            let
                tutorialMessage =
                    case step of
                        TutorialBasicMove ->
                            t.tutorialBasicMove

                        TutorialBoardSelection ->
                            t.tutorialBoardSelection

                        TutorialWinningSmall ->
                            t.tutorialWinningSmall

                        TutorialWinningBig ->
                            t.tutorialWinningBig

                        TutorialFreeChoice ->
                            t.tutorialFreeChoice

                stepNumber =
                    case step of
                        TutorialBasicMove ->
                            1

                        TutorialBoardSelection ->
                            2

                        TutorialWinningSmall ->
                            3

                        TutorialFreeChoice ->
                            4

                        TutorialWinningBig ->
                            5

                overlayStyles =
                    case step of
                        TutorialWinningBig ->
                            [ style "bottom" "70%"
                            , style "transform" "translate(-50%, 0%)"
                            ]

                        _ ->
                            [ style "bottom" "0px"
                            , style "left" "0"
                            , style "right" "0"
                            , style "margin-left" "auto"
                            , style "margin-right" "auto"
                            ]

                isNextEnabled =
                    shouldEnableNextButton model

                buttons =
                    if isNextEnabled then
                        [ button
                            [ style "padding" "10px 20px"
                            , style "font-size" "0.9em"
                            , style "background-color" Color.primary
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "6px"
                            , style "cursor" "pointer"
                            , style "transition" "all 0.2s ease"
                            , onClick NextTutorialStep
                            ]
                            [ text t.nextStep ]
                        ]

                    else
                        []
            in
            div
                ([ style "position" "fixed"
                 , style "left" "50%"
                 , style "background-color" (Color.withAlpha c.background 0.85)
                 , style "padding" "20px"
                 , style "border-radius" "15px"
                 , style "box-shadow" "0 4px 12px rgba(0, 0, 0, 0.2)"
                 , style "max-width" "90%"
                 , style "width" "600px"
                 , style "text-align" "center"
                 , style "animation" "slideIn 0.3s ease-out"
                 , style "z-index" "1000"
                 , style "backdrop-filter" "blur(5px)"
                 ]
                    ++ overlayStyles
                )
                [ div
                    [ style "margin-bottom" "10px"
                    , style "color" c.text
                    , style "font-size" "0.8em"
                    ]
                    [ text (String.fromInt stepNumber ++ "/6") ]
                , p
                    [ style "margin" "0 0 15px 0"
                    , style "color" c.text
                    , style "font-size" "1em"
                    , style "line-height" "1.5"
                    ]
                    [ text tutorialMessage ]
                , div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    , style "justify-content" "center"
                    ]
                    buttons
                ]

        _ ->
            text ""


viewGameResultModal : UserConfig -> FrontendModel -> Html FrontendMsg
viewGameResultModal ({ t, c } as userConfig) model =
    div
        Modal.overlay
        [ div
            (Modal.container c)
            [ T.h2 c
                (case model.gameResult of
                    Won ->
                        t.youWon

                    Lost ->
                        t.youLost

                    Drew ->
                        t.draw

                    Ongoing ->
                        ""
                )
            , button
                (Button.primary userConfig ++
                    [ onClick ReturnToMenu
                    , Dom.idToAttribute (Dom.id "back-to-menu-button")
                    ]
                )
                [ text t.backToMenu ]
            ]
        ]


viewLoadingScreen : UserConfig -> FrontendModel -> Html FrontendMsg
viewLoadingScreen { c, t } model =
    div
        (Container.fullscreen ++
            Layout.flexCenter ++
            [ style "z-index" "2"
            ] ++
            (if model.isLoading then
                Animation.fadeIn
             else
                Animation.fadeOut
            )
        )
        [ div
            (Layout.flexColumnCenter ++
                Animation.pulse
            )
            [ T.loadingTitle c "Ultimate"
            , T.loadingSubtitle c "Tic-Tac-Toe"
            , T.loadingAuthor c "By Charlon"
            ]
        ]


isDark : UserPreference -> Mode -> Bool
isDark preference systemMode =
    case preference of
        DarkMode ->
            True

        LightMode ->
            False

        SystemMode ->
            systemMode == Dark
