module Frontend exposing (..)

import Admin exposing (view)
import Audio exposing (Sound(..))
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
        , checkSmallBoardWinner
        , emptySmallBoard
        , getAllAvailableMoves
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
import Palette.Animation as Animation
import Palette.Button as Button
import Palette.Container as Container
import Palette.Layout as Layout
import Palette.Modal as Modal
import Palette.Typography as T
import Palette.Utils as Utils
import Random
import Random.Extra
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
    let
        initModel : FrontendModel
        initModel =
            { key = key
            , route = route
            , localStorage = LocalStorage.default
            , seed = Nothing
            , rulesModalVisible = False
            , frClickCount = 0
            , debuggerVisible = False
            , debuggerPosition = { x = 0, y = 0 }
            , isDraggingDebugger = False
            , dragOffset = { x = 0, y = 0 }
            , debuggerSize = { width = 300, height = 200 }
            , isResizingDebugger = False
            , selectedDifficulty = Nothing
            , showAbandonConfirm = False
            , tutorialState = Nothing
            , botDifficultyMenuOpen = False
            , inMatchmaking = False
            , isLoading = True
            , backendModel = Nothing
            , frontendGame = Nothing
            }

        route =
            case url.path of
                "/admin" ->
                    AdminRoute

                "/" ->
                    HomeRoute

                _ ->
                    HomeRoute
    in
    ( initModel
    , Command.batch
        [ LocalStorage.getLocalStorage
        , startLoadingAnimation
        , Effect.Lamdera.sendToBackend RequestBackendModelToBackend
        , Effect.Task.perform GotTime Effect.Time.now
        ]
    )


startLoadingAnimation : Command FrontendOnly ToBackend FrontendMsg
startLoadingAnimation =
    Effect.Process.sleep (Duration.milliseconds 1000)
        |> Effect.Task.perform (\_ -> LoadingComplete)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
update msg ({ localStorage } as model) =
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
            let
                newRoute =
                    case url.path of
                        "/admin" ->
                            AdminRoute

                        "/" ->
                            HomeRoute

                        _ ->
                            HomeRoute
            in
            ( { model | route = newRoute }
            , if newRoute == AdminRoute then
                Effect.Lamdera.sendToBackend RequestBackendModelToBackend

              else
                Command.none
            )

        CellClicked move ->
            model.frontendGame
                |> Maybe.map
                    (\frontendGame ->
                        if frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1 then
                            ( model, Command.none )

                        else
                            case model.route of
                                GameRoute (WithBot difficulty) ->
                                    handlePlayerMove frontendGame model (WithBot difficulty) move
                                        |> (\( updatedFrontendGame, cmd ) ->
                                                ( { model | frontendGame = Just { updatedFrontendGame | botIsPlaying = True } }
                                                , Command.batch
                                                    [ cmd
                                                    , Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 0))
                                                    ]
                                                )
                                           )

                                GameRoute WithFriend ->
                                    handlePlayerMove frontendGame model WithFriend move
                                        |> (\( updatedFrontendGame, cmd ) ->
                                                ( { model | frontendGame = Just updatedFrontendGame }
                                                , cmd
                                                )
                                           )

                                GameRoute OnlineGameMode ->
                                    handlePlayerMove frontendGame model OnlineGameMode move
                                        |> (\( updatedFrontendGame, cmd ) ->
                                                ( { model | frontendGame = Just updatedFrontendGame }
                                                , cmd
                                                )
                                           )

                                _ ->
                                    ( model, Command.none )
                    )
                |> Maybe.withDefault ( model, Command.none )

        BotMove ->
            case ( model.route |> Debug.log "route", model.frontendGame |> Debug.log "frontendGame" ) of
                ( GameRoute (WithBot difficulty), Just game ) ->
                    let
                        botMove =
                            Bot.findBestMove game difficulty |> Debug.log "botMove"

                        ( newFrontendGame, cmd ) =
                            case botMove of
                                Just move ->
                                    handlePlayerMove game model (WithBot difficulty) move

                                Nothing ->
                                    ( game, Command.none )
                    in
                    ( { model | frontendGame = Just { newFrontendGame | botIsPlaying = False } }, cmd )

                _ ->
                    ( model, Command.none )

        RestartGame ->
            ( { model
                | frontendGame = Nothing
              }
            , Command.none
            )

        StartGameWithFriend ->
            ( { model
                | route = GameRoute WithFriend
                , frontendGame = Just <| initialFrontendGame Nothing FriendOpponent False
              }
            , Audio.playSound ButtonClickSound
            )

        StartGameWithBot ->
            ( { model | botDifficultyMenuOpen = True }
            , Audio.playSound ButtonClickSound
            )

        SelectBotDifficulty difficulty ->
            ( { model
                | selectedDifficulty = Just difficulty
              }
            , Audio.playSound ButtonClickSound
            )

        StartBotGame difficulty firstPlayer ->
            let
                ( botIsPlaying, newSeed ) =
                    case firstPlayer of
                        HumanBegins ->
                            ( False, model.seed )

                        BotBegins ->
                            ( True, model.seed )

                        RandomBegins ->
                            Random.step Random.Extra.bool (model.seed |> Maybe.withDefault (Random.initialSeed 42))
                                |> Tuple.mapSecond Just

                self =
                    if botIsPlaying then
                        O

                    else
                        X
            in
            ( { model
                | route = GameRoute <| WithBot difficulty
                , frontendGame = Just <| initialFrontendGame (Just self) (BotOpponent difficulty) botIsPlaying
                , seed = newSeed
              }
            , Command.batch
                [ Audio.playSound ButtonClickSound
                , if botIsPlaying then
                    Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 0))

                  else
                    Command.none
                ]
            )

        GotTime time ->
            ( { model | seed = Just <| Random.initialSeed (Effect.Time.posixToMillis time) }
            , Command.none
            )

        ReturnToMenu ->
            ( { model
                | route = HomeRoute
                , frontendGame = Nothing
                , tutorialState = Nothing
              }
            , Audio.playSound ButtonClickSound
            )

        CancelBotDifficulty ->
            ( { model | botDifficultyMenuOpen = False }
            , Audio.playSound ButtonClickSound
            )

        PlayForMeAgainstTheBot ->
            case ( model.route, model.frontendGame ) of
                ( GameRoute (WithBot _), Just frontendGame ) ->
                    let
                        ( newModel, cmd ) =
                            case Bot.findBestMove frontendGame Elite |> Debug.log "botMove" of
                                Just move ->
                                    let
                                        ( newFrontendGame, moveCmd ) =
                                            handlePlayerMove frontendGame model (WithBot Elite) move |> Debug.log "botMove2"

                                        botIsPlaying =
                                            (newFrontendGame.gameResult == Nothing) |> Debug.log "botIsPlaying"
                                    in
                                    ( { model | frontendGame = Just { newFrontendGame | botIsPlaying = botIsPlaying } }
                                    , if botIsPlaying then
                                        Command.batch
                                            [ moveCmd
                                            , Effect.Task.perform (always BotMove) (Effect.Process.sleep (Duration.milliseconds 0))
                                            ]

                                      else
                                        moveCmd
                                    )

                                Nothing ->
                                    ( model |> Debug.log "!!!!!!!!!", Command.none )
                    in
                    ( newModel, cmd )

                _ ->
                    ( model, Command.none )

        ChangeLanguage lang ->
            let
                newLocalStorage =
                    { localStorage | language = lang }
            in
            ( { model
                | localStorage = newLocalStorage
                , frClickCount =
                    if lang == FR then
                        model.frClickCount + 1

                    else
                        model.frClickCount
              }
            , Command.batch
                [ LocalStorage.storeValue (LocalStorage.LanguageUpdate lang)
                , Audio.playSound ButtonClickSound
                ]
            )

        CloseDebugger ->
            ( { model | debuggerVisible = False, frClickCount = 0 }, Command.none )

        UndoMove ->
            model.frontendGame
                |> Maybe.map
                    (\frontendGame ->
                        if frontendGame.currentMoveIndex >= 0 then
                            let
                                newIndex =
                                    (frontendGame.currentMoveIndex - 1)
                                        |> Debug.log "newIndex"

                                newFrontendGame =
                                    reconstructBoardFromMoves frontendGame.moveHistory newIndex frontendGame
                            in
                            ( { model
                                | frontendGame = Just newFrontendGame
                              }
                            , Command.none
                            )

                        else
                            ( model, Command.none )
                    )
                |> Maybe.withDefault ( model, Command.none )

        RedoMove ->
            model.frontendGame
                |> Maybe.map
                    (\frontendGame ->
                        if frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1 then
                            let
                                newIndex =
                                    frontendGame.currentMoveIndex + 1

                                newFrontendGame =
                                    reconstructBoardFromMoves frontendGame.moveHistory newIndex frontendGame
                            in
                            ( { model | frontendGame = Just newFrontendGame }
                            , Command.none
                            )

                        else
                            ( model, Command.none )
                    )
                |> Maybe.withDefault ( model, Command.none )

        ToggleDarkMode ->
            let
                newPreference =
                    case localStorage.userPreference of
                        DarkMode ->
                            LightMode

                        LightMode ->
                            SystemMode

                        SystemMode ->
                            DarkMode
            in
            ( { model | localStorage = { localStorage | userPreference = newPreference } }
            , Command.batch
                [ LocalStorage.storeValue (LocalStorage.ThemePreferenceUpdate newPreference)
                , Audio.playSound ButtonClickSound
                ]
            )

        ToggleSound ->
            let
                newLocalStorage =
                    { localStorage | soundEnabled = not localStorage.soundEnabled }
            in
            ( { model | localStorage = newLocalStorage }
            , LocalStorage.storeValue (LocalStorage.SoundUpdate newLocalStorage.soundEnabled)
            )

        ToggleDebugMode ->
            ( model, Command.none )

        ReceivedLocalStorage localStorage_ ->
            ( { model | localStorage = localStorage_ }
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
            , Audio.playSound ButtonClickSound
            )

        StartOnlineGame ->
            ( { model | inMatchmaking = True }
            , Command.batch
                [ Effect.Lamdera.sendToBackend JoinMatchmakingToBackend
                , Audio.playSound PlayOnlineSound
                ]
            )

        LeaveMatchmaking ->
            ( { model | inMatchmaking = False }
            , Effect.Lamdera.sendToBackend LeaveMatchmakingToBackend
            )

        Tick newTime ->
            ( model, Command.none )

        ShowAbandonConfirm ->
            ( { model | showAbandonConfirm = True }, Command.none )

        HideAbandonConfirm ->
            ( { model | showAbandonConfirm = False }, Command.none )

        ConfirmAbandon frontendGame ->
            ( { model
                | showAbandonConfirm = False
                , frontendGame = Just { frontendGame | gameResult = Just Lost }
              }
            , Command.batch
                [ Effect.Lamdera.sendToBackend AbandonGameToBackend
                , Audio.playSound LoseSound
                ]
            )

        StartTutorial ->
            ( { model
                | tutorialState = Just TutorialStep1
                , frontendGame = Just (getTutorialBoard TutorialStep1)
                , route = TutorialRoute
                , rulesModalVisible = False
              }
            , Audio.playSound ButtonClickSound
            )

        NextTutorialStep ->
            case model.tutorialState of
                Just tutorialState ->
                    let
                        nextStep =
                            case tutorialState of
                                TutorialStep1 ->
                                    Just TutorialStep2

                                TutorialStep2 ->
                                    Just TutorialStep3

                                TutorialStep3 ->
                                    Just TutorialStep4

                                TutorialStep4 ->
                                    Just TutorialStep5

                                TutorialStep5 ->
                                    Just TutorialStep6

                                TutorialStep6 ->
                                    Nothing
                    in
                    ( { model
                        | tutorialState = nextStep
                        , frontendGame = Maybe.map getTutorialBoard nextStep
                      }
                    , Audio.playSound ButtonClickSound
                    )

                _ ->
                    ( model, Command.none )

        CompleteTutorial ->
            ( { model
                | tutorialState = Nothing
                , route = HomeRoute
                , frontendGame = Nothing
              }
            , Audio.playSound ButtonClickSound
            )

        LoadingComplete ->
            ( { model | isLoading = False }, Command.none )

        KeyLeft ->
            update UndoMove model

        KeyRight ->
            update RedoMove model


initialFrontendGame : Maybe Player -> Opponent -> Bool -> FrontendGame
initialFrontendGame self opponent botIsPlaying =
    { self = self
    , boards = List.repeat 9 emptySmallBoard
    , currentPlayer = X
    , activeBoard = Nothing
    , winner = Nothing
    , lastMove = Nothing
    , moveHistory = []
    , currentMoveIndex = -1
    , gameResult = Nothing
    , botIsPlaying = botIsPlaying
    , opponent = opponent
    }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )

        GameFoundToFrontend { opponentId, playerRole } ->
            ( { model
                | route = GameRoute OnlineGameMode
                , frontendGame = Just (initialFrontendGame (Just playerRole) (OnlineOpponent opponentId) False)
                , inMatchmaking = False
              }
            , Audio.playSound ButtonClickSound
            )

        OpponentMoveToFrontend move ->
            model.frontendGame
                |> Maybe.map
                    (\frontendGame ->
                        let
                            newFrontendGame =
                                makeMove
                                    (reconstructBoardFromMoves frontendGame.moveHistory (List.length frontendGame.moveHistory) frontendGame)
                                    move
                                    frontendGame.currentPlayer

                            soundCommand =
                                let
                                    oldSmallBoard =
                                        List.getAt move.boardIndex newFrontendGame.boards
                                            |> Maybe.withDefault GameLogic.emptySmallBoard

                                    newSmallBoard =
                                        List.getAt move.boardIndex newFrontendGame.boards
                                            |> Maybe.withDefault GameLogic.emptySmallBoard
                                in
                                if newSmallBoard.winner /= Nothing && oldSmallBoard.winner == Nothing then
                                    Audio.playSound SmallWinSound

                                else
                                    Audio.playSound (MoveSound frontendGame.currentPlayer)
                        in
                        ( { model
                            | frontendGame =
                                Just
                                    { newFrontendGame
                                        | gameResult =
                                            case newFrontendGame.winner of
                                                Just winner ->
                                                    if frontendGame.self == Just winner then
                                                        Just Won

                                                    else
                                                        Just Lost

                                                Nothing ->
                                                    if isBigBoardComplete newFrontendGame.boards then
                                                        Just Draw

                                                    else
                                                        Nothing
                                        , currentMoveIndex = List.length newFrontendGame.moveHistory
                                        , moveHistory = newFrontendGame.moveHistory ++ [ move ]
                                    }
                          }
                        , soundCommand
                        )
                    )
                |> Maybe.withDefault ( model, Command.none )

        OpponentLeftToFrontend frontendGame ->
            ( { model
                | frontendGame = Just { frontendGame | gameResult = Just Won }

                -- TODO: handle this to display a message to the user --FIXME
              }
            , Audio.playSound WinSound
            )

        BackendModelReceivedToFrontend backendModel ->
            ( { model | backendModel = Just backendModel }, Command.none )

        SendGameToFrontend frontendGame ->
            ( { model | frontendGame = Just frontendGame, route = GameRoute OnlineGameMode, inMatchmaking = False }, Command.none )



--  canPlayInMiniBoard =
--             case frontendGame.activeBoard of
--                 Nothing ->
--                     True
--                 Just activeBoardIndex ->
--                     boardIndex == activeBoardIndex
--         smallBoard =
--             List.getAt boardIndex frontendGame.boards
--         isCellEmpty =
--             case smallBoard of
--                 Just sb ->
--                     case List.getAt cellIndex sb.cells of
--                         Just cell ->
--                             cell == Empty
--                         Nothing ->
--                             False
--                 Nothing ->
--                     False
--         isSmallBoardAvailable =
--             case smallBoard of
--                 Just sb ->
--                     sb.winner == Nothing && not (List.all (\cell -> cell /= Empty) sb.cells)
--                 Nothing ->
--                     False


isValidMove : FrontendGame -> Move -> Bool
isValidMove frontendGame move =
    case List.getAt move.boardIndex frontendGame.boards of
        Just sb ->
            let
                canPlayInMiniBoard =
                    case frontendGame.activeBoard of
                        Nothing ->
                            True

                        Just activeBoardIndex ->
                            move.boardIndex == activeBoardIndex

                isCellEmpty =
                    case List.getAt move.cellIndex sb.cells of
                        Just cell ->
                            cell == Empty

                        Nothing ->
                            False

                isSmallBoardAvailable =
                    sb.winner == Nothing && not (List.all (\cell -> cell /= Empty) sb.cells)
            in
            canPlayInMiniBoard && isCellEmpty && isSmallBoardAvailable

        Nothing ->
            False


handlePlayerMove : FrontendGame -> FrontendModel -> GameMode -> Move -> ( FrontendGame, Command FrontendOnly ToBackend FrontendMsg )
handlePlayerMove frontendGame model gameMode move =
    let
        newFrontendGame =
            makeMove frontendGame move frontendGame.currentPlayer

        newGameResult =
            case model.route of
                GameRoute (WithBot _) ->
                    case newFrontendGame.winner of
                        Just winner ->
                            if frontendGame.self == Just winner then
                                Just Won

                            else
                                Just Lost

                        Nothing ->
                            if isBigBoardComplete newFrontendGame.boards then
                                Just Draw

                            else
                                Nothing

                GameRoute OnlineGameMode ->
                    case newFrontendGame.winner of
                        Just winner ->
                            if frontendGame.self == Just winner then
                                Just Won

                            else
                                Just Lost

                        Nothing ->
                            if isBigBoardComplete newFrontendGame.boards then
                                Just Draw

                            else
                                Nothing

                _ ->
                    Nothing

        updatedFrontendGame =
            { newFrontendGame
                | gameResult = newGameResult
                , currentMoveIndex = List.length newFrontendGame.moveHistory
                , moveHistory = newFrontendGame.moveHistory ++ [ move ]
            }

        soundCommand =
            case newGameResult of
                Just Won ->
                    Audio.playSound WinSound

                Just Lost ->
                    Audio.playSound LoseSound

                Just Draw ->
                    Audio.playSound DrawSound

                Nothing ->
                    case
                        List.getAt move.boardIndex newFrontendGame.boards
                            |> Maybe.andThen .winner
                    of
                        Just _ ->
                            Audio.playSound SmallWinSound

                        Nothing ->
                            Audio.playSound (MoveSound frontendGame.currentPlayer)
    in
    if isValidMove frontendGame move then
        case gameMode of
            WithBot _ ->
                ( updatedFrontendGame
                , soundCommand
                )

            OnlineGameMode ->
                case frontendGame.self of
                    Just _ ->
                        ( updatedFrontendGame
                        , Command.batch
                            [ soundCommand
                            , Effect.Lamdera.sendToBackend <| MakeMoveToBackend move
                            ]
                        )

                    Nothing ->
                        ( updatedFrontendGame, soundCommand )

            WithFriend ->
                ( updatedFrontendGame, soundCommand )

    else
        ( frontendGame, Audio.playSound ErrorSound )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        ({ c, t } as userConfig) =
            { t = translations model.localStorage.language
            , c = themes model.localStorage.userPreference model.localStorage.systemMode
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
                        body {
                            font-family: 'Press Start 2P', cursive;
                        }
                        .game-symbol {
                            font-family: Arial, sans-serif;
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
                 , case ( model.route, model.frontendGame, model.tutorialState ) of
                    ( HomeRoute, _, _ ) ->
                        viewHome userConfig model

                    ( TutorialRoute, Just frontendGame, Just tutorialState ) ->
                        Tutorial.View.viewGame userConfig frontendGame model tutorialState

                    ( GameRoute mode, Just frontendGame, _ ) ->
                        viewGame userConfig model frontendGame mode

                    ( AdminRoute, _, _ ) ->
                        Admin.view userConfig model.backendModel

                    _ ->
                        text "SHOULD NOT HAPPEN"
                 ]
                    ++ Debugger.view userConfig model
                    ++ [ case model.frontendGame |> Maybe.andThen .gameResult of
                            Just gameResult ->
                                viewGameResultModal userConfig gameResult

                            Nothing ->
                                text ""
                       , if model.rulesModalVisible then
                            viewRulesModal userConfig model

                         else
                            text ""
                       , case ( model.frontendGame, model.tutorialState ) of
                            ( Just frontendGame, Just tutorialState ) ->
                                Tutorial.View.viewTutorialOverlay userConfig frontendGame tutorialState

                            _ ->
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
        (Button.secondary userConfig
            ++ Utils.margin10
            ++ Utils.fullWidth
            ++ Utils.maxWidth 300
            ++ [ onClick msg
               , Dom.idToAttribute htmlId
               ]
        )
        [ text label ]


playOnlineButton : UserConfig -> FrontendModel -> Html FrontendMsg
playOnlineButton ({ t } as userConfig) model =
    div
        Layout.flexColumnCenter
        [ button
            (Button.primary userConfig
                ++ Button.withShadow
                ++ (if model.inMatchmaking then
                        Button.disabled

                    else
                        []
                   )
                ++ Utils.margin10
                ++ Utils.fullWidth
                ++ Utils.maxWidth 300
                ++ [ onClick
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


viewHome : UserConfig -> FrontendModel -> Html FrontendMsg
viewHome ({ t, c } as userConfig) model =
    div
        (Container.card c
            ++ [ style "text-align" "center"
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
                , playOnlineButton userConfig model
                , viewGameButton userConfig (Dom.id "gameWithBot") t.playWithBot StartGameWithBot
                , viewGameButton userConfig (Dom.id "toggleRules") t.rulesTitle ToggleRulesModal
                , viewGameButton userConfig (Dom.id "gameWithFriend") t.playWithFriend StartGameWithFriend
                ]
        , if model.rulesModalVisible then
            viewRulesModal userConfig model

          else
            text ""
        ]


viewGame : UserConfig -> FrontendModel -> FrontendGame -> GameMode -> Html FrontendMsg
viewGame ({ t, c } as userConfig) model frontendGame mode =
    let
        gameTitle =
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

                OnlineGameMode ->
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
            , viewStatus userConfig frontendGame mode
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
                [ case model.tutorialState of
                    Just tutorialState ->
                        Tutorial.View.viewBigBoardTutorial userConfig frontendGame tutorialState

                    Nothing ->
                        viewBigBoard userConfig frontendGame
                ]
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
                                (if frontendGame.self == Just frontendGame.currentPlayer && frontendGame.winner == Nothing && not frontendGame.botIsPlaying then
                                    Color.primary

                                 else
                                    Color.disabled
                                )
                            , style "color" "white"
                            , style "border" "none"
                            , style "border-radius" "10px"
                            , style "cursor"
                                (if frontendGame.self == Just frontendGame.currentPlayer && frontendGame.winner == Nothing && not frontendGame.botIsPlaying then
                                    "pointer"

                                 else
                                    "not-allowed"
                                )
                            , style "transition" "all 0.2s ease"
                            , style "margin-bottom" "10px"
                            , style "width" "100%"
                            , onClick PlayForMeAgainstTheBot
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
                                    , onClick (ConfirmAbandon frontendGame)
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

                OnlineGameMode ->
                    case frontendGame.opponent of
                        OnlineOpponent _ ->
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
                                        , onClick (ConfirmAbandon frontendGame)
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

                        _ ->
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
            , div
                [ style "display" "flex"
                , style "gap" "10px"
                ]
                [ button
                    [ style "flex" "1"
                    , style "padding" "8px"
                    , style "font-size" "1.2em"
                    , style "background-color"
                        (if frontendGame.currentMoveIndex >= 0 then
                            Color.primary

                         else
                            Color.disabled
                        )
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    , style "cursor"
                        (if frontendGame.currentMoveIndex >= 0 then
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
                        (if frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1 then
                            Color.primary

                         else
                            Color.disabled
                        )
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "6px"
                    , style "cursor"
                        (if frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1 then
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
            Just difficulty ->
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
                        , onClick (StartBotGame difficulty HumanBegins)
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
                        , onClick (StartBotGame difficulty BotBegins)
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
                        , onClick (StartBotGame difficulty RandomBegins)
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


viewStatus : UserConfig -> FrontendGame -> GameMode -> Html FrontendMsg
viewStatus ({ t, c } as userConfig) frontendGame mode =
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
                case frontendGame.gameResult of
                    Just Won ->
                        t.youWon

                    Just Lost ->
                        t.youLost

                    Just Draw ->
                        t.draw

                    Nothing ->
                        case frontendGame.winner of
                            Just winner ->
                                case mode of
                                    OnlineGameMode ->
                                        if frontendGame.self == Just winner then
                                            t.youWon

                                        else
                                            t.youLost

                                    _ ->
                                        case winner of
                                            X ->
                                                t.playerXWins

                                            O ->
                                                t.playerOWins

                            Nothing ->
                                if isBigBoardComplete frontendGame.boards then
                                    t.draw

                                else
                                    case mode of
                                        WithBot _ ->
                                            if frontendGame.botIsPlaying then
                                                "🤖"

                                            else
                                                t.yourTurn

                                        OnlineGameMode ->
                                            case frontendGame.self of
                                                Just player ->
                                                    if player == frontendGame.currentPlayer then
                                                        t.yourTurn

                                                    else
                                                        t.enemyTurn

                                                Nothing ->
                                                    t.waitingForOpponent

                                        WithFriend ->
                                            if frontendGame.currentPlayer == X then
                                                t.playerXTurn

                                            else
                                                t.playerOTurn
            , if frontendGame.botIsPlaying then
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
        , viewLanguageButton "FR" FR (model.localStorage.language == FR) (isDark model.localStorage.userPreference model.localStorage.systemMode)
        , viewLanguageButton "EN" EN (model.localStorage.language == EN) (isDark model.localStorage.userPreference model.localStorage.systemMode)
        ]


viewSoundButton : UserConfig -> FrontendModel -> Html FrontendMsg
viewSoundButton ({ t, c } as userConfig) model =
    div
        [ style "position" "absolute"
        , style "top" "5px"
        , style "left" "20px"
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
        [ button
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
            , onClick ToggleSound
            ]
            [ text
                (if model.localStorage.soundEnabled then
                    "🔊"

                 else
                    "🔇"
                )
            ]
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
            (case model.localStorage.userPreference of
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
        [ lang |> languageToString |> String.toUpper |> text ]


viewBigBoard : UserConfig -> FrontendGame -> Html FrontendMsg
viewBigBoard ({ t, c } as userConfig) frontendGame =
    let
        isBotTurn =
            frontendGame.botIsPlaying

        isOpponentTurn =
            case frontendGame.self of
                Just player ->
                    player /= frontendGame.currentPlayer

                Nothing ->
                    False

        isViewingHistory =
            frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1

        shouldBlink =
            not isViewingHistory
                && ((isBotTurn && frontendGame.activeBoard == Nothing)
                        || (isOpponentTurn && frontendGame.activeBoard == Nothing)
                   )

        boards =
            frontendGame.boards

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
            List.indexedMap (viewSmallBoard userConfig frontendGame) boards

        blinkStyle =
            if shouldBlink then
                [ style "animation" "bigBoardBlink 1s ease-in-out infinite" ]

            else
                []
    in
    div
        (boardStyle ++ blinkStyle)
        boardElements


viewSmallBoard : UserConfig -> FrontendGame -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard ({ t, c } as userConfig) frontendGame boardIndex smallBoardData =
    let
        isActive =
            case frontendGame.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    boardIndex == activeBoardIndex

        isBotTurn =
            frontendGame.botIsPlaying

        isOpponentTurn =
            case frontendGame.self of
                Just player ->
                    player /= frontendGame.currentPlayer

                Nothing ->
                    False

        isViewingHistory =
            frontendGame.currentMoveIndex < List.length frontendGame.moveHistory - 1

        isClickable =
            isActive
                && not isBotTurn
                && not isOpponentTurn
                && not isViewingHistory

        borderColor =
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
                    c.background

        cellStyle =
            [ style "box-shadow" ("inset 0 0 0 1px " ++ c.border) ]

        shouldBlink =
            not isViewingHistory
                && ((isActive && isBotTurn && frontendGame.activeBoard /= Nothing)
                        || (isActive && isOpponentTurn)
                   )

        cellElements =
            List.indexedMap (viewCell userConfig frontendGame boardIndex isClickable cellStyle) smallBoardData.cells

        blinkStyle =
            if shouldBlink then
                [ style "animation" "blink 1s ease-in-out infinite" ]

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
        )
        cellElements


viewCell : UserConfig -> FrontendGame -> Int -> Bool -> List (Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewCell ({ t, c } as userConfig) frontendGame boardIndex isClickable cellStyles cellIndex cellState =
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
                case frontendGame.currentPlayer of
                    X ->
                        "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 100 100' fill='none' stroke='%23dc3545' stroke-width='10' stroke-linecap='round'%3E%3Cline x1='20' y1='20' x2='80' y2='80'/%3E%3Cline x1='80' y1='20' x2='20' y2='80'/%3E%3C/svg%3E\") 12 12, pointer"

                    O ->
                        "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='24' height='24' viewBox='0 0 100 100' fill='none' stroke='%230d6efd' stroke-width='10' stroke-linecap='round'%3E%3Ccircle cx='50' cy='50' r='35'/%3E%3C/svg%3E\") 12 12, pointer"

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

        isLastMove =
            case frontendGame.lastMove of
                Just move ->
                    move.boardIndex == boardIndex && move.cellIndex == cellIndex

                Nothing ->
                    False

        lastMoveHighlight =
            if isLastMove then
                [ style "box-shadow" "inset 0 0 0 2px #4CAF50"
                , style "background-color" (c.background ++ "CC") -- Add some transparency
                ]

            else
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
            ++ lastMoveHighlight
            ++ (if isCellClickable then
                    [ onClick (CellClicked { boardIndex = boardIndex, cellIndex = cellIndex }) ]

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
        , if model.tutorialState == Nothing then
            Effect.Browser.Events.onKeyDown
                (D.map
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                KeyLeft

                            "ArrowRight" ->
                                KeyRight

                            _ ->
                                NoOp
                    )
                    (D.field "key" D.string)
                )

          else
            Subscription.none
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


reconstructBoardFromMoves : List Move -> Int -> FrontendGame -> FrontendGame
reconstructBoardFromMoves moves upToIndex initialBoardState =
    let
        movesToApply =
            List.take (upToIndex + 1) moves

        freshBoard : FrontendGame
        freshBoard =
            { initialBoardState
                | boards = List.repeat 9 emptySmallBoard
                , botIsPlaying = initialBoardState.botIsPlaying
                , currentMoveIndex = upToIndex
                , activeBoard = Nothing
                , lastMove = Nothing
                , currentPlayer = X
            }
    in
    List.foldl
        (\move board ->
            GameLogic.makeMove board move board.currentPlayer
        )
        freshBoard
        movesToApply


switchPlayer : Player -> Player
switchPlayer player =
    case player of
        X ->
            O

        O ->
            X


viewRulesModal : UserConfig -> FrontendModel -> Html FrontendMsg
viewRulesModal ({ t, c } as userConfig) model =
    div
        (Modal.overlay
            ++ [ onClick ToggleRulesModal ]
        )
        [ div
            (Modal.container c
                ++ [ Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True )) ]
            )
            [ T.h2 c t.rulesTitle
            , T.pre c t.rulesText
            , div
                Layout.flexCenterWithGap
                [ button
                    (Button.primary userConfig
                        ++ [ onClick StartTutorial ]
                    )
                    [ text t.startTutorial ]
                , button
                    (Button.primary userConfig
                        ++ [ onClick ToggleRulesModal ]
                    )
                    [ text t.close ]
                ]
            ]
        ]


viewGameResultModal : UserConfig -> GameResult -> Html FrontendMsg
viewGameResultModal ({ t, c } as userConfig) gameResult =
    div
        Modal.overlay
        [ div
            (Modal.container c)
            [ T.h2 c
                (case gameResult of
                    Won ->
                        t.youWon

                    Lost ->
                        t.youLost

                    Draw ->
                        t.draw
                )
            , button
                (Button.primary userConfig
                    ++ [ onClick ReturnToMenu
                       , Dom.idToAttribute (Dom.id "back-to-menu-button")
                       ]
                )
                [ text t.backToMenu ]
            ]
        ]


viewLoadingScreen : UserConfig -> FrontendModel -> Html FrontendMsg
viewLoadingScreen { c } model =
    div
        (Container.fullscreen
            ++ Layout.flexCenter
            ++ [ style "z-index" "2"
               ]
            ++ (if model.isLoading then
                    Animation.fadeIn

                else
                    Animation.fadeOut
               )
        )
        [ div
            (Layout.flexColumnCenter
                ++ Animation.pulse
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
