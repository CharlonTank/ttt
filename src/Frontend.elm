module Frontend exposing (..)

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
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (Language(..), Translation, languageToString, stringToLanguage, translations)
import Json.Decode as D
import Json.Encode as E
import Lamdera
import List.Extra as List
import String
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes
import Theme exposing (DarkOrLight(..), boolToDarkOrLight, darkModeToString, stringToDarkOrLight, themes)
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
      , language = EN
      , darkMode = Dark
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
      , t = translations EN
      , c = Theme.themes Dark
      }
    , Command.batch
        [ Command.sendToJs "getLocalStorageValue" getLocalStorageValue_ (E.string "language")
        , Command.sendToJs "getLocalStorageValue" getLocalStorageValue_ (E.string "darkMode")
        ]
    )


initialBoard : Player -> BigBoard
initialBoard startingPlayer =
    { boards = List.repeat 9 emptySmallBoard
    , currentPlayer = startingPlayer
    , activeBoard = Nothing
    , winner = Nothing
    , initialPlayer = startingPlayer
    }


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }


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

                                            newGameResult =
                                                case modelAfterMove.board.winner of
                                                    Just winner ->
                                                        if winner == X then
                                                            Won

                                                        else
                                                            Lost

                                                    Nothing ->
                                                        if isBigBoardComplete modelAfterMove.board then
                                                            Drew

                                                        else
                                                            Ongoing
                                        in
                                        ( { modelAfterMove
                                            | botThinking = False
                                            , gameResult = newGameResult
                                          }
                                        , moveCmd
                                        )

                                    Nothing ->
                                        ( { model | botThinking = False }, Command.none )
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
            , Command.none
            )

        StartGameWithBot ->
            ( { model | botDifficultyMenuOpen = True }, Command.none )

        SelectBotDifficulty difficulty ->
            ( { model
                | selectedDifficulty = Just difficulty
              }
            , Command.none
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

        ReturnToMenu ->
            ( { model
                | route = Home
                , gameResult = Ongoing
              }
            , Command.none
            )

        CancelBotDifficulty ->
            ( { model | botDifficultyMenuOpen = False }, Command.none )

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
                        | language = lang
                        , t = translations lang
                        , frClickCount =
                            if lang == FR then
                                model.frClickCount + 1

                            else
                                model.frClickCount
                    }
            in
            ( newModel
            , Command.sendToJs
                "storeLocalStorage"
                storeLocalStorage_
                (E.object
                    [ ( "key", E.string "language" ), ( "value", E.string (languageToString lang) ) ]
                )
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
                newDarkMode =
                    if model.darkMode == Dark then
                        Light

                    else
                        Dark
            in
            ( { model
                | darkMode = newDarkMode
                , c = Theme.themes newDarkMode
              }
            , Command.sendToJs
                "storeLocalStorage"
                storeLocalStorage_
                (E.object
                    [ ( "key", E.string "darkMode" ), ( "value", E.string (darkModeToString newDarkMode) ) ]
                )
            )

        ToggleDebugMode ->
            ( model, Command.none )

        ReceivedLocalStorage json ->
            let
                language =
                    json
                        |> D.decodeValue (D.field "language" D.string)
                        |> Result.map stringToLanguage
                        |> Result.withDefault EN

                darkMode =
                    json
                        |> D.decodeValue (D.field "darkMode" D.string)
                        |> Result.map stringToDarkOrLight
                        |> Result.withDefault Dark
            in
            ( { model
                | language = language
                , darkMode = darkMode
                , t = language |> translations
                , c = themes model.darkMode
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

        ReceivedLocalStorageValue value_ ->
            let
                decodedValue =
                    D.decodeValue D.string value_
                        |> Result.map
                            (\str ->
                                case D.decodeString localStorageValueDecoder str of
                                    Ok { key, value } ->
                                        case key of
                                            "language" ->
                                                { model | language = stringToLanguage value, t = translations (stringToLanguage value) }

                                            "darkMode" ->
                                                { model | darkMode = stringToDarkOrLight value, c = themes (stringToDarkOrLight value) }

                                            _ ->
                                                model

                                    Err _ ->
                                        model
                            )
                        |> Debug.log "decodedValue"
                        |> Result.withDefault model
            in
            ( decodedValue, Command.none )

        ToggleRulesModal ->
            ( { model | rulesModalVisible = not model.rulesModalVisible }, Command.none )

        StartOnlineGame ->
            ( { model | inMatchmaking = True }
            , Effect.Lamdera.sendToBackend JoinMatchmaking
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
            , Effect.Lamdera.sendToBackend AbandonGame
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
            , Command.none
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
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        CompleteTutorial ->
            ( { model
                | tutorialState = Nothing
                , route = Home
                , board = initialBoard X
              }
            , Command.none
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
    in
    if canPlayInBoard && isCellEmpty && isSmallBoardAvailable then
        case model.route of
            Game (WithBot difficulty) ->
                if model.board.currentPlayer == X then
                    ( updatedModel
                    , Effect.Process.sleep (Duration.milliseconds 500)
                        |> Effect.Task.perform (\_ -> BotMove)
                    )

                else
                    ( updatedModel, Command.none )

            Game OnlineGame ->
                case model.onlinePlayer of
                    Just player ->
                        if player == model.board.currentPlayer then
                            ( updatedModel
                            , Effect.Lamdera.sendToBackend (MakeMove boardIndex cellIndex player)
                            )

                        else
                            ( model, Command.none )

                    Nothing ->
                        ( model, Command.none )

            _ ->
                ( updatedModel, Command.none )

    else
        ( model, Command.none )


hasWinningPattern : List a -> a -> Bool
hasWinningPattern cells player =
    let
        winningPatterns =
            [ [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]
            , [ 0, 4, 8 ]
            , [ 2, 4, 6 ]
            ]

        isWinningPattern pattern =
            List.all
                (\index ->
                    List.getAt index cells
                        |> Maybe.map ((==) player)
                        |> Maybe.withDefault False
                )
                pattern
    in
    List.any isWinningPattern winningPatterns


findBestMove : BigBoard -> BotDifficulty -> Maybe ( Int, Int )
findBestMove board difficulty =
    let
        availableMoves =
            getAllAvailableMoves board

        depthForDifficulty =
            case difficulty of
                Easy ->
                    2

                Medium ->
                    3

                Hard ->
                    4

                Elite ->
                    5

        moveScores =
            List.map
                (\( boardIdx, cellIdx ) ->
                    let
                        newBoard =
                            makeMove board boardIdx cellIdx board.currentPlayer

                        baseScore =
                            alphabeta newBoard depthForDifficulty -10000 10000 False

                        randomFactor =
                            case difficulty of
                                Easy ->
                                    modBy 200 (boardIdx * 17 + cellIdx * 13)

                                Medium ->
                                    modBy 100 (boardIdx * 11 + cellIdx * 7)

                                Hard ->
                                    modBy 50 (boardIdx * 7 + cellIdx * 5)

                                Elite ->
                                    modBy 10 (boardIdx * 3 + cellIdx * 2)
                    in
                    ( ( boardIdx, cellIdx ), baseScore + randomFactor )
                )
                availableMoves

        shouldChooseRandom =
            case difficulty of
                Easy ->
                    modBy 3 (List.length availableMoves) == 0

                Medium ->
                    modBy 5 (List.length availableMoves) == 0

                Hard ->
                    modBy 10 (List.length availableMoves) == 0

                Elite ->
                    modBy 20 (List.length availableMoves) == 0

        bestMove =
            if shouldChooseRandom then
                List.getAt (modBy (List.length availableMoves) (List.length moveScores)) availableMoves

            else
                List.sortBy Tuple.second moveScores
                    |> List.reverse
                    |> List.head
                    |> Maybe.map Tuple.first
    in
    bestMove


alphabeta : BigBoard -> Int -> Int -> Int -> Bool -> Int
alphabeta board depth alpha beta isMaximizing =
    case board.winner of
        Just winner ->
            if winner == O then
                1000 + depth

            else
                -1000 - depth

        Nothing ->
            if depth == 0 then
                evaluatePosition board O

            else if isMaximizing then
                alphabetaMax board depth alpha beta

            else
                alphabetaMin board depth alpha beta


alphabetaMax : BigBoard -> Int -> Int -> Int -> Int
alphabetaMax board depth alpha beta =
    let
        availableMoves =
            getAllAvailableMoves board

        helper moves currentAlpha bestScore =
            case moves of
                [] ->
                    bestScore

                ( boardIdx, cellIdx ) :: rest ->
                    let
                        newBoard =
                            makeMove board boardIdx cellIdx board.currentPlayer

                        score =
                            alphabeta newBoard (depth - 1) currentAlpha beta False

                        newBestScore =
                            Basics.max bestScore score

                        newAlpha =
                            Basics.max currentAlpha newBestScore
                    in
                    if beta <= newAlpha then
                        newBestScore

                    else
                        helper rest newAlpha newBestScore
    in
    helper availableMoves alpha -10000


alphabetaMin : BigBoard -> Int -> Int -> Int -> Int
alphabetaMin board depth alpha beta =
    let
        availableMoves =
            getAllAvailableMoves board

        helper moves currentBeta bestScore =
            case moves of
                [] ->
                    bestScore

                ( boardIdx, cellIdx ) :: rest ->
                    let
                        newBoard =
                            makeMove board boardIdx cellIdx board.currentPlayer

                        score =
                            alphabeta newBoard (depth - 1) alpha currentBeta True

                        newBestScore =
                            Basics.min bestScore score

                        newBeta =
                            Basics.min currentBeta newBestScore
                    in
                    if newBeta <= alpha then
                        newBestScore

                    else
                        helper rest newBeta newBestScore
    in
    helper availableMoves beta 10000


evaluatePosition : BigBoard -> Player -> Int
evaluatePosition board forPlayer =
    let
        evaluateSmallBoard : SmallBoard -> Int
        evaluateSmallBoard smallBoard =
            let
                evaluateLine : List CellState -> Int
                evaluateLine line =
                    let
                        playerCount =
                            List.filter (\cell -> cell == Filled forPlayer) line
                                |> List.length

                        opponentCount =
                            List.filter
                                (\cell ->
                                    case cell of
                                        Filled p ->
                                            p /= forPlayer

                                        Empty ->
                                            False
                                )
                                line
                                |> List.length

                        emptyCount =
                            List.filter ((==) Empty) line
                                |> List.length
                    in
                    if playerCount == 3 then
                        100

                    else if opponentCount == 3 then
                        -100

                    else if playerCount == 2 && emptyCount == 1 then
                        20

                    else if opponentCount == 2 && emptyCount == 1 then
                        -20

                    else if playerCount == 1 && emptyCount == 2 then
                        2

                    else if opponentCount == 1 && emptyCount == 2 then
                        -2

                    else
                        0

                rows =
                    [ List.take 3 smallBoard.cells
                    , List.take 3 (List.drop 3 smallBoard.cells)
                    , List.drop 6 smallBoard.cells
                    ]

                cols =
                    [ List.map (\i -> List.getAt (i * 3) smallBoard.cells |> Maybe.withDefault Empty) (List.range 0 2)
                    , List.map (\i -> List.getAt (i * 3 + 1) smallBoard.cells |> Maybe.withDefault Empty) (List.range 0 2)
                    , List.map (\i -> List.getAt (i * 3 + 2) smallBoard.cells |> Maybe.withDefault Empty) (List.range 0 2)
                    ]

                diags =
                    [ List.map (\i -> List.getAt (i * 4) smallBoard.cells |> Maybe.withDefault Empty) (List.range 0 2)
                    , List.map (\i -> List.getAt (i * 2 + 2) smallBoard.cells |> Maybe.withDefault Empty) (List.range 0 2)
                    ]

                allLines =
                    rows ++ cols ++ diags
            in
            List.sum (List.map evaluateLine allLines)

        boardScores =
            List.map evaluateSmallBoard board.boards

        centerBoardBonus =
            case List.getAt 4 board.boards of
                Just centerBoard ->
                    case centerBoard.winner of
                        Just winner ->
                            if winner == forPlayer then
                                200

                            else
                                -200

                        Nothing ->
                            let
                                centerCellBonus =
                                    case List.getAt 4 centerBoard.cells of
                                        Just (Filled player) ->
                                            if player == forPlayer then
                                                50

                                            else
                                                -50

                                        _ ->
                                            0
                            in
                            centerCellBonus

                Nothing ->
                    0

        cornerBoardsBonus =
            let
                cornerIndexes =
                    [ 0, 2, 6, 8 ]

                cornerBoards =
                    List.filterMap (\i -> List.getAt i board.boards) cornerIndexes

                cornerScore smallBoard =
                    case smallBoard.winner of
                        Just winner ->
                            if winner == forPlayer then
                                100

                            else
                                -100

                        Nothing ->
                            0
            in
            List.sum (List.map cornerScore cornerBoards)

        strategicBonus =
            case board.activeBoard of
                Just nextBoardIndex ->
                    if nextBoardIndex == 4 then
                        -50

                    else if List.member nextBoardIndex [ 0, 2, 6, 8 ] then
                        -30

                    else
                        20

                Nothing ->
                    0
    in
    List.sum boardScores + centerBoardBonus + cornerBoardsBonus + strategicBonus


getAllAvailableMoves : BigBoard -> List ( Int, Int )
getAllAvailableMoves board =
    let
        validBoardIndexes =
            case board.activeBoard of
                Nothing ->
                    List.range 0 8

                Just idx ->
                    [ idx ]

        isValidBoardAndCell boardIdx cellIdx =
            isValidMove board boardIdx cellIdx
    in
    List.concatMap
        (\boardIdx ->
            List.map (\cellIdx -> ( boardIdx, cellIdx ))
                (List.range 0 8)
        )
        validBoardIndexes
        |> List.filter (\( boardIdx, cellIdx ) -> isValidBoardAndCell boardIdx cellIdx)


isValidMove : BigBoard -> Int -> Int -> Bool
isValidMove board boardIndex cellIndex =
    case board.winner of
        Just _ ->
            False

        Nothing ->
            let
                targetBoard =
                    List.drop boardIndex board.boards |> List.head |> Maybe.withDefault emptySmallBoard

                targetCell =
                    List.drop cellIndex targetBoard.cells |> List.head |> Maybe.withDefault Empty
            in
            targetBoard.winner == Nothing && targetCell == Empty


makeMove : BigBoard -> Int -> Int -> Player -> BigBoard
makeMove board boardIndex cellIndex player =
    let
        updateCell cells index =
            List.indexedMap
                (\i cell ->
                    if i == index then
                        Filled player

                    else
                        cell
                )
                cells

        updateBoard boards index =
            List.indexedMap
                (\i smallBoard ->
                    if i == index then
                        let
                            updatedCells =
                                updateCell smallBoard.cells cellIndex
                        in
                        { smallBoard
                            | cells = updatedCells
                            , winner = checkWinner updatedCells
                        }

                    else
                        smallBoard
                )
                boards

        updatedBoards =
            updateBoard board.boards boardIndex

        nextPlayer =
            case player of
                X ->
                    O

                O ->
                    X

        nextActiveBoard =
            if isSmallBoardComplete (List.drop cellIndex updatedBoards |> List.head |> Maybe.withDefault emptySmallBoard) then
                Nothing

            else
                Just cellIndex
    in
    { board
        | boards = updatedBoards
        , currentPlayer = nextPlayer
        , activeBoard = nextActiveBoard
        , winner = checkBigBoardWinner updatedBoards
    }


isSmallBoardComplete : SmallBoard -> Bool
isSmallBoardComplete board =
    board.winner /= Nothing || List.all ((/=) Empty) board.cells


checkWinner : List CellState -> Maybe Player
checkWinner cells =
    let
        winningCombinations =
            [ [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]
            , [ 0, 4, 8 ]
            , [ 2, 4, 6 ]
            ]

        checkCombination : List Int -> Maybe Player
        checkCombination indices =
            case List.map (\i -> List.getAt i cells) indices of
                [ Just (Filled p1), Just (Filled p2), Just (Filled p3) ] ->
                    if p1 == p2 && p2 == p3 then
                        Just p1

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap checkCombination winningCombinations
        |> List.head


checkBigBoardWinner : List SmallBoard -> Maybe Player
checkBigBoardWinner boards =
    let
        boardWinners =
            List.map .winner boards

        winningCombinations =
            [ [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]
            , [ 0, 4, 8 ]
            , [ 2, 4, 6 ]
            ]

        getWinner index =
            List.drop index boardWinners |> List.head |> Maybe.withDefault Nothing

        checkCombination indexes =
            case List.map getWinner indexes of
                (Just player) :: rest ->
                    if List.all ((==) (Just player)) rest then
                        Just player

                    else
                        Nothing

                _ ->
                    Nothing
    in
    List.filterMap checkCombination winningCombinations
        |> List.head


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Command restriction toMsg FrontendMsg )
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
            in
            ( { model
                | board = newBoard
                , moveHistory = newHistory
                , currentMoveIndex = newIndex
                , gameResult = newGameResult
              }
            , Command.none
            )

        OpponentLeft ->
            ( { model
                | onlineOpponent = Nothing
                , onlinePlayer = Nothing
                , gameResult = Won
              }
            , Command.none
            )


view : FrontendModel -> Browser.Document FrontendMsg
view ({ t, c } as model) =
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
            """
            ]
        , div
            [ style "min-height" "100vh"
            , style "min-height" "100dvh"
            , style "width" "100%"
            , style "background" c.gradientBackground
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "padding" "env(safe-area-inset-top, 10px) env(safe-area-inset-right, 10px) env(safe-area-inset-bottom, 10px) env(safe-area-inset-left, 10px)"
            , style "box-sizing" "border-box"
            , style "position" "relative"
            , style "letter-spacing" "1px"
            , style "line-height" "1.5"
            ]
            ([ viewLanguageSelector model
             , case model.route of
                Home ->
                    viewHome model

                Game mode ->
                    viewGame model mode
             ]
                ++ Debugger.view model
                ++ [ if model.gameResult /= Ongoing then
                        viewGameResultModal model

                     else
                        text ""
                   , if model.rulesModalVisible then
                        viewRulesModal model

                     else
                        text ""
                   , if model.tutorialState /= Nothing then
                        viewTutorialOverlay model

                     else
                        text ""
                   ]
            )
        ]
    }


viewGameButton : HtmlId -> FrontendModel -> String -> FrontendMsg -> Html FrontendMsg
viewGameButton htmlId ({ c } as model) label msg =
    button
        [ style "padding" "15px 20px"
        , style "font-size" "0.8em"
        , style "font-family" "inherit"
        , style "background-color" c.secondaryBackground
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "10px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.2)"
        , style "margin" "10px"
        , style "width" "100%"
        , style "max-width" "300px"
        , onClick msg
        , Dom.idToAttribute htmlId
        ]
        [ text label ]


viewHome : FrontendModel -> Html FrontendMsg
viewHome ({ t, c } as model) =
    div
        [ style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "padding" "40px"
        , style "text-align" "center"
        , style "max-width" "400px"
        , style "width" "90%"
        , style "background-color" c.background
        , style "color" c.text
        ]
        [ if model.botDifficultyMenuOpen then
            viewBotDifficultyMenu model

          else
            div []
                [ h1
                    [ style "margin" "0 0 20px 0"
                    , style "color" c.text
                    , style "font-size" "1.5em"
                    ]
                    [ text t.welcome ]
                , p
                    [ style "color" c.text
                    , style "margin-bottom" "30px"
                    , style "line-height" "1.6"
                    , style "font-size" "0.7em"
                    ]
                    [ text t.description ]
                , div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "align-items" "center"
                    , style "gap" "15px"
                    ]
                    [ viewGameButton (Dom.id "gameWithFriend") model t.playWithFriend StartGameWithFriend
                    , viewGameButton (Dom.id "gameWithBot") model t.playWithBot StartGameWithBot
                    , viewGameButton (Dom.id "toggleRules") model t.rulesTitle ToggleRulesModal
                    , button
                        [ class "menu-button"
                        , onClick
                            (if model.inMatchmaking then
                                LeaveMatchmaking

                             else
                                StartOnlineGame
                            )
                        , style "padding" "15px 20px"
                        , style "font-size" "0.8em"
                        , style "font-family" "inherit"
                        , style "background-color" c.secondaryBackground
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "10px"
                        , style "cursor" "pointer"
                        , style "transition" "all 0.2s ease"
                        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.2)"
                        , style "margin" "10px"
                        , style "width" "100%"
                        , style "max-width" "300px"
                        , style "opacity"
                            (if model.inMatchmaking then
                                "0.7"

                             else
                                "1"
                            )
                        ]
                        [ if model.inMatchmaking then
                            div
                                [ style "display" "flex"
                                , style "align-items" "center"
                                , style "justify-content" "center"
                                , style "gap" "8px"
                                ]
                                [ text t.searching
                                , viewThinkingIndicator
                                ]

                          else
                            text t.playOnline
                        ]
                    ]
                ]
        , if model.rulesModalVisible then
            viewRulesModal model

          else
            text ""
        ]


viewGame : FrontendModel -> GameMode -> Html FrontendMsg
viewGame ({ t, c } as model) mode =
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
            , viewStatus model
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
                [ viewBigBoard model ]
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


viewBotDifficultyMenu : FrontendModel -> Html FrontendMsg
viewBotDifficultyMenu ({ t, c } as model) =
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
        , viewDifficultyButton model t.easy Easy
        , viewDifficultyButton model t.medium Medium
        , viewDifficultyButton model t.hard Hard
        , viewDifficultyButton model t.elite Elite
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


viewDifficultyButton : FrontendModel -> String -> BotDifficulty -> Html FrontendMsg
viewDifficultyButton ({ c } as model) label difficulty =
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


viewStatus : FrontendModel -> Html msg
viewStatus ({ t, c } as model) =
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


viewLanguageSelector : FrontendModel -> Html FrontendMsg
viewLanguageSelector ({ c } as model) =
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
        [ viewDarkModeButton model
        , div [ style "width" "1px", style "height" "20px", style "background-color" c.border ] []
        , viewLanguageButton "FR" FR (model.language == FR) (model.darkMode == Dark)
        , viewLanguageButton "EN" EN (model.language == EN) (model.darkMode == Dark)
        ]


viewDarkModeButton : FrontendModel -> Html FrontendMsg
viewDarkModeButton ({ c } as model) =
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
            (if model.darkMode == Dark then
                "🌙"

             else
                "☀️"
            )
        ]


viewLanguageButton : String -> Language -> Bool -> Bool -> Html FrontendMsg
viewLanguageButton label lang isActive isDark =
    button
        [ style "padding" "8px 12px"
        , style "font-size" "0.7em"
        , style "font-family" "inherit"
        , style "background-color"
            (if isActive then
                Color.primary

             else if isDark then
                Color.darkSecondaryBackground

             else
                Color.lightBorder
            )
        , style "color"
            (if isActive then
                "white"

             else if isDark then
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


viewBigBoard : FrontendModel -> Html FrontendMsg
viewBigBoard ({ c } as model) =
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
            List.indexedMap (viewSmallBoard model) boards

        blinkStyle =
            if shouldBlink then
                [ style "animation" "bigBoardBlink 1s ease-in-out infinite" ]

            else
                []
    in
    div
        (boardStyle ++ blinkStyle)
        boardElements


viewSmallBoard : FrontendModel -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard ({ c } as model) boardIndex smallBoardData =
    let
        isActive =
            case model.board.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    activeBoardIndex == boardIndex

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
            List.indexedMap (viewCell model boardIndex isClickable cellStyle) smallBoardData.cells

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


viewCell : FrontendModel -> Int -> Bool -> List (Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewCell ({ c } as model) boardIndex isClickable cellStyles cellIndex cellState =
    case model.tutorialState of
        Just _ ->
            Tutorial.View.viewTutorialCell model
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



--Subscription.fromJs : String -> ((Json.Decode.Value -> msg) -> Sub msg) -> (Json.Decode.Value -> msg) -> Subscription FrontendOnly msg


subscriptions : FrontendModel -> Subscription FrontendOnly FrontendMsg
subscriptions model =
    Subscription.batch
        [ receiveLocalStorage ReceivedLocalStorage
        , Subscription.fromJs "receiveLocalStorageValue_"
            receiveLocalStorageValue_
            ReceivedLocalStorageValue
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


storeLocalStorage : { key : String, value : String } -> Cmd FrontendMsg
storeLocalStorage { key, value } =
    storeLocalStorage_
        (E.object
            [ ( "key", E.string key )
            , ( "value", E.string value )
            ]
        )



-- copyToClipboard : (Json.Decode.value -> msg) -> Subscription FrontendOnly msg
-- copyToClipboard msg =
--     Subscription.fromJs "scrollEventPort" scrollEventPort msg


receiveLocalStorage : (D.Value -> msg) -> Subscription FrontendOnly msg
receiveLocalStorage msg =
    Subscription.fromJs "receiveLocalStorage_"
        receiveLocalStorage_
        msg


localStorageDecoder : D.Decoder LocalStorage
localStorageDecoder =
    D.map2
        (\lang dark ->
            { language = stringToLanguage lang
            , darkMode = boolToDarkOrLight dark
            }
        )
        (D.field "language" D.string)
        (D.field "darkMode" D.bool)


localStorageValueDecoder : D.Decoder { key : String, value : String }
localStorageValueDecoder =
    D.map2 (\k v -> { key = k, value = v })
        (D.field "key" D.string)
        (D.field "value" D.string)


reconstructBoardFromMoves : List Move -> Int -> BigBoard -> BigBoard
reconstructBoardFromMoves moves upToIndex initialBoardState =
    let
        movesToApply =
            List.take (upToIndex + 1) moves

        freshBoard =
            { boards = List.repeat 9 emptySmallBoard
            , currentPlayer = initialBoardState.initialPlayer
            , activeBoard = Nothing
            , winner = Nothing
            , initialPlayer = initialBoardState.initialPlayer
            }
    in
    List.foldl
        (\move board ->
            makeMove board move.boardIndex move.cellIndex board.currentPlayer
        )
        freshBoard
        movesToApply


viewRulesModal : FrontendModel -> Html FrontendMsg
viewRulesModal ({ t, c } as model) =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background-color" "rgba(0, 0, 0, 0.5)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "z-index" "1000"
        , onClick ToggleRulesModal
        ]
        [ div
            [ style "background-color" c.background
            , style "padding" "30px"
            , style "border-radius" "15px"
            , style "text-align" "center"
            , style "width" "min(100%, 1200px)"
            , style "animation" "slideIn 0.3s ease-out"
            , Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True ))
            ]
            [ h2
                [ style "margin" "0 0 20px 0"
                , style "font-size" "1.2em"
                , style "color" c.text
                ]
                [ text t.rulesTitle ]
            , pre
                [ style "color" c.text
                , style "white-space" "pre-wrap"
                , style "font-family" "inherit"
                , style "text-align" "left"
                , style "line-height" "1.6"
                , style "margin" "0 0 20px 0"
                , style "font-size" "0.7em"
                ]
                [ text t.rulesText ]
            , div
                [ style "display" "flex"
                , style "gap" "10px"
                , style "justify-content" "center"
                ]
                [ button
                    [ style "padding" "12px 30px"
                    , style "font-size" "0.8em"
                    , style "background-color" Color.primary
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "8px"
                    , style "cursor" "pointer"
                    , style "transition" "all 0.2s ease"
                    , onClick StartTutorial
                    ]
                    [ text t.startTutorial ]
                , button
                    [ style "padding" "12px 30px"
                    , style "font-size" "0.8em"
                    , style "background-color" Color.primary
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "8px"
                    , style "cursor" "pointer"
                    , style "transition" "all 0.2s ease"
                    , onClick ToggleRulesModal
                    ]
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
                                |> Maybe.withDefault emptySmallBoard
                    in
                    List.getAt 2 centerBoard.cells == Just (Filled X)

                TutorialBoardSelection ->
                    True

                TutorialWinningSmall ->
                    let
                        centerBoard =
                            List.getAt 4 model.board.boards
                                |> Maybe.withDefault emptySmallBoard
                    in
                    centerBoard.winner == Just X

                TutorialFreeChoice ->
                    True

                TutorialWinningBig ->
                    model.board.winner == Just X

        Nothing ->
            False


viewTutorialOverlay : FrontendModel -> Html FrontendMsg
viewTutorialOverlay ({ t, c } as model) =
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


isBigBoardComplete : BigBoard -> Bool
isBigBoardComplete board =
    List.all isSmallBoardComplete board.boards


viewGameResultModal : FrontendModel -> Html FrontendMsg
viewGameResultModal ({ t, c } as model) =
    div
        [ style "position" "fixed"
        , style "top" "0"
        , style "left" "0"
        , style "width" "100%"
        , style "height" "100%"
        , style "background-color" "rgba(0, 0, 0, 0.7)"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "z-index" "1000"
        ]
        [ div
            [ style "background-color" c.background
            , style "padding" "30px"
            , style "border-radius" "15px"
            , style "text-align" "center"
            , style "animation" "slideIn 0.3s ease-out"
            ]
            [ h2
                [ style "margin" "0 0 20px 0"
                , style "font-size" "1.2em"
                , style "color" c.text
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
                            ""
                ]
            , button
                [ style "padding" "12px 30px"
                , style "font-size" "0.8em"
                , style "background-color" Color.primary
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "8px"
                , style "cursor" "pointer"
                , style "transition" "all 0.2s ease"
                , onClick ReturnToMenu
                ]
                [ text t.backToMenu ]
            ]
        ]
