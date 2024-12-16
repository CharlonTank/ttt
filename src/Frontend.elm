module Frontend exposing (..)

import Bot
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Color
import Debug
import Debugger
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (Translation, translations)
import Json.Decode as D
import Json.Encode as E
import Lamdera
import List.Extra as List
import Process
import String
import Svg exposing (Svg, circle, line, svg)
import Svg.Attributes
import Task
import Time
import Tutorial.Tutorial exposing (getTutorialBoard, isTutorialMoveValid)
import Tutorial.Types exposing (TutorialStep(..))
import Tutorial.View exposing (viewTutorialCell)
import Types exposing (..)
import Url




app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , board = initialBoard X
      , route = Home
      , language = EN
      , darkMode = False
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
      , localStorageValues = Dict.empty
      , selectedDifficulty = Nothing
      , onlinePlayer = Nothing
      , showAbandonConfirm = False
      , gameResult = Ongoing
      , tutorialState = Nothing
      , botDifficultyMenuOpen = False
      , botThinking = False
      , inMatchmaking = False
      , onlineOpponent = Nothing
      }
    , Cmd.batch
        [ getLocalStorageValue_ "language"
        , getLocalStorageValue_ "darkMode"
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


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        CellClicked boardIndex cellIndex ->
            if model.currentMoveIndex < List.length model.moveHistory - 1 then
                -- We're viewing history, don't allow moves
                ( model, Cmd.none )

            else
                case model.route of
                    Game (WithBot difficulty) ->
                        if model.board.currentPlayer == O then
                            -- It's bot's turn, don't allow player to move
                            ( model, Cmd.none )

                        else
                            let
                                ( updatedModel, cmd ) =
                                    handlePlayerMove model boardIndex cellIndex
                            in
                            if updatedModel.board.winner == Nothing then
                                -- Schedule bot's move and show thinking state
                                ( { updatedModel | botThinking = True }
                                , Cmd.batch [ cmd, Task.perform (always BotMove) (Process.sleep 500) ]
                                )
                            else
                                ( updatedModel, cmd )

                    Game WithFriend ->
                        handlePlayerMove model boardIndex cellIndex

                    Game OnlineGame ->
                        if model.onlineOpponent == Nothing then
                            ( model, Cmd.none )

                        else
                            let
                                isMyTurn =
                                    case model.onlinePlayer of
                                        Just player ->
                                            player == model.board.currentPlayer

                                        Nothing ->
                                            False
                            in
                            if not isMyTurn then
                                ( model, Cmd.none )

                            else
                                let
                                    ( updatedModel, cmd ) =
                                        handlePlayerMove model boardIndex cellIndex
                                in
                                ( updatedModel
                                , Cmd.batch
                                    [ cmd
                                    , Lamdera.sendToBackend (MakeMove boardIndex cellIndex model.board.currentPlayer)
                                    ]
                                )

                    Home ->
                        ( model, Cmd.none )

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
                                        ( { model | botThinking = False }, Cmd.none )
                        in
                        ( newModel, cmd )

                    else
                        ( model, Cmd.none )

                Game WithFriend ->
                    ( model, Cmd.none )

                Game OnlineGame ->
                    ( model, Cmd.none )

                Home ->
                    ( model, Cmd.none )

        RestartGame ->
            ( { model
                | board = initialBoard X
                , moveHistory = []
                , currentMoveIndex = -1
              }
            , Cmd.none
            )

        StartGameWithFriend ->
            ( { model
                | route = Game WithFriend
                , board = initialBoard X
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Ongoing
              }
            , Cmd.none
            )

        StartGameWithBot ->
            ( { model | botDifficultyMenuOpen = True }, Cmd.none )

        SelectBotDifficulty difficulty ->
            ( { model
                | selectedDifficulty = Just difficulty
              }
            , Cmd.none
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
                        Task.perform (always BotMove) (Process.sleep 500)

                    else
                        Cmd.none
            in
            case model.route of
                Game (WithBot _) ->
                    -- If we're already in a game, restart with new starting player
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
                    -- If we're selecting difficulty, start the game
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
                            ( model, Cmd.none )

        ReturnToMenu ->
            ( { model
                | route = Home
                , gameResult = Ongoing
              }
            , Cmd.none
            )

        CancelBotDifficulty ->
            ( { model | botDifficultyMenuOpen = False }, Cmd.none )

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
                                            -- Schedule bot's move and show thinking state
                                            ( { modelAfterMove | botThinking = True }
                                            , Cmd.batch [ moveCmd, Task.perform (always BotMove) (Process.sleep 500) ]
                                            )

                                        else
                                            ( modelAfterMove, moveCmd )

                                    Nothing ->
                                        ( model, Cmd.none )
                        in
                        ( newModel, cmd )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeLanguage lang ->
            let
                newModel =
                    { model
                        | language = lang
                        , frClickCount =
                            if lang == FR then
                                model.frClickCount + 1

                            else
                                model.frClickCount
                    }
            in
            ( newModel
            , storeLocalStorage { key = "language", value = languageToString lang }
            )

        CloseDebugger ->
            ( { model | debuggerVisible = False, frClickCount = 0 }, Cmd.none )

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
                , Cmd.none
                )

            else
                ( model, Cmd.none )

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
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }
            , storeLocalStorage { key = "darkMode", value = boolToString (not model.darkMode) }
            )

        ToggleDebugMode ->
            ( model, Cmd.none )

        ReceivedLocalStorage { language, darkMode } ->
            ( { model | language = stringToLanguage language, darkMode = darkMode }
            , Cmd.none
            )

        StartDraggingDebugger mouseX mouseY ->
            ( { model
                | isDraggingDebugger = True
                , dragOffset =
                    { x = mouseX - model.debuggerPosition.x
                    , y = mouseY - model.debuggerPosition.y
                    }
              }
            , Cmd.none
            )

        StopDraggingDebugger ->
            ( { model | isDraggingDebugger = False }, Cmd.none )

        DragDebugger mouseX mouseY ->
            if model.isDraggingDebugger then
                ( { model
                    | debuggerPosition =
                        { x = mouseX - model.dragOffset.x
                        , y = mouseY - model.dragOffset.y
                        }
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        StartResizingDebugger ->
            ( { model | isResizingDebugger = True }, Cmd.none )

        StopResizingDebugger ->
            ( { model | isResizingDebugger = False }, Cmd.none )

        ResizeDebugger mouseX mouseY ->
            if model.isResizingDebugger then
                let
                    newWidth =
                        mouseX - model.debuggerPosition.x

                    newHeight =
                        mouseY - model.debuggerPosition.y

                    -- Minimum size constraints
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
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ReceivedLocalStorageValue key value ->
            ( { model | localStorageValues = Dict.insert key value model.localStorageValues }
            , Cmd.none
            )

        ToggleRulesModal ->
            ( { model | rulesModalVisible = not model.rulesModalVisible }, Cmd.none )

        StartOnlineGame ->
            ( { model | inMatchmaking = True }
            , Lamdera.sendToBackend JoinMatchmaking
            )

        ReceivedGameFound data ->
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
            , Cmd.none
            )

        ReceivedOpponentMove move ->
            let
                newBoard =
                    makeMove model.board move.boardIndex move.cellIndex

                -- Create new move record and update history
                newHistory =
                    List.take (model.currentMoveIndex + 1) model.moveHistory ++ [ move ]

                newIndex =
                    List.length newHistory - 1

                -- Determine game result after opponent's move
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
            , Cmd.none
            )

        ReceivedOpponentLeft ->
            ( { model
                | onlineOpponent = Nothing
                , onlinePlayer = Nothing
                , gameResult = Won
              }
            , Cmd.none
            )

        StartWithRandomPlayer ->
            ( model, Task.perform GotTime Time.now )

        GotTime time ->
            let
                randomValue =
                    modBy 2 (Time.posixToMillis time)

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
                        Task.perform (always BotMove) (Process.sleep 500)

                    else
                        Cmd.none
            in
            case model.route of
                Game (WithBot _) ->
                    -- If we're already in a game, restart with new starting player
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
                    -- If we're selecting difficulty, start the game
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
                            ( model, Cmd.none )

        Tick newTime ->
            ( model, Cmd.none )

        ShowAbandonConfirm ->
            ( { model | showAbandonConfirm = True }, Cmd.none )

        HideAbandonConfirm ->
            ( { model | showAbandonConfirm = False }, Cmd.none )

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
            , Lamdera.sendToBackend AbandonGame
            )

        StartTutorial ->
            ( { model
                | tutorialState = Just TutorialIntro
                , board = getTutorialBoard TutorialIntro
                , route = Game WithFriend
                , moveHistory = []
                , currentMoveIndex = -1
                , gameResult = Ongoing
                , rulesModalVisible = False
              }
            , Cmd.none
            )

        NextTutorialStep ->
            case model.tutorialState of
                Just tutorialState ->
                    let
                        nextStep =
                            case tutorialState of
                                TutorialIntro ->
                                    Just TutorialBasicMove

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
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SkipTutorial ->
            ( { model
                | tutorialState = Nothing
                , route = Home
                , board = initialBoard X
              }
            , Cmd.none
            )

        CompleteTutorial ->
            ( { model
                | tutorialState = Nothing
                , route = Home
                , board = initialBoard X
              }
            , Cmd.none
            )


handlePlayerMove : FrontendModel -> Int -> Int -> ( FrontendModel, Cmd FrontendMsg )
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
            makeMove board boardIndex cellIndex

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
                                && -- Check if it's the center cell of the center board
                                   (let
                                        boardAfterMove =
                                            makeMove board boardIndex cellIndex

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
                                && -- Check if it's the winning move
                                   (let
                                        boardAfterMove =
                                            makeMove board boardIndex cellIndex
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
                    Just TutorialIntro ->
                        Just TutorialBasicMove

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
                    , Process.sleep 500
                        |> Task.perform (\_ -> BotMove)
                    )

                else
                    ( updatedModel, Cmd.none )

            Game OnlineGame ->
                case model.onlinePlayer of
                    Just player ->
                        if player == model.board.currentPlayer then
                            ( updatedModel
                            , Lamdera.sendToBackend (MakeMove boardIndex cellIndex player)
                            )

                        else
                            ( model, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            _ ->
                ( updatedModel, Cmd.none )

    else
        ( model, Cmd.none )


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
                            makeMove board boardIdx cellIdx

                        baseScore =
                            alphabeta newBoard depthForDifficulty -10000 10000 False

                        -- Add randomness based on difficulty
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

                        -- Petit facteur alatoire pour Elite
                    in
                    ( ( boardIdx, cellIdx ), baseScore + randomFactor )
                )
                availableMoves

        -- For Easy and Medium, sometimes choose a random move
        shouldChooseRandom =
            case difficulty of
                Easy ->
                    modBy 3 (List.length availableMoves) == 0

                -- 33% chance
                Medium ->
                    modBy 5 (List.length availableMoves) == 0

                -- 20% chance
                Hard ->
                    modBy 10 (List.length availableMoves) == 0

                -- 10% chance
                Elite ->
                    modBy 20 (List.length availableMoves) == 0

        -- 5% chance
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
                -- Bot wins (O)

            else
                -1000 - depth

        -- Player wins (X)
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
                            makeMove board boardIdx cellIdx

                        score =
                            alphabeta newBoard (depth - 1) currentAlpha beta False

                        newBestScore =
                            Basics.max bestScore score

                        newAlpha =
                            Basics.max currentAlpha newBestScore
                    in
                    if beta <= newAlpha then
                        -- Beta cutoff
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
                            makeMove board boardIdx cellIdx

                        score =
                            alphabeta newBoard (depth - 1) alpha currentBeta True

                        newBestScore =
                            Basics.min bestScore score

                        newBeta =
                            Basics.min currentBeta newBestScore
                    in
                    if newBeta <= alpha then
                        -- Alpha cutoff
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
                        -- Winning line

                    else if opponentCount == 3 then
                        -100
                        -- Opponent winning line

                    else if playerCount == 2 && emptyCount == 1 then
                        20
                        -- Two in a row with potential

                    else if opponentCount == 2 && emptyCount == 1 then
                        -20
                        -- Block opponent's potential win

                    else if playerCount == 1 && emptyCount == 2 then
                        2
                        -- One with potential

                    else if opponentCount == 1 && emptyCount == 2 then
                        -2
                        -- Opponent one with potential

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

        -- Add bonus for center board control
        centerBoardBonus =
            case List.getAt 4 board.boards of
                Just centerBoard ->
                    case centerBoard.winner of
                        Just winner ->
                            if winner == forPlayer then
                                200
                                -- Increased importance of center

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

        -- Add bonus for corner boards control
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
                                -- Increased importance of corners

                            else
                                -100

                        Nothing ->
                            0
            in
            List.sum (List.map cornerScore cornerBoards)

        -- Add bonus for strategic moves that send opponent to unfavorable boards
        strategicBonus =
            case board.activeBoard of
                Just nextBoardIndex ->
                    if nextBoardIndex == 4 then
                        -50
                        -- Penalize sending opponent to center

                    else if List.member nextBoardIndex [ 0, 2, 6, 8 ] then
                        -30
                        -- Penalize sending opponent to corners

                    else
                        20

                -- Bonus for sending opponent to less strategic boards
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


makeMove : BigBoard -> Int -> Int -> BigBoard
makeMove board boardIndex cellIndex =
    let
        updateCell cells index =
            List.indexedMap
                (\i cell ->
                    if i == index then
                        Filled board.currentPlayer

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
            case board.currentPlayer of
                X ->
                    O

                O ->
                    X

        -- The next active board is determined by the cell that was just played
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
            [ -- Rows
              [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]

            -- Columns
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]

            -- Diagonals
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


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        GameFound data ->
            update (ReceivedGameFound data) model

        OpponentMove move ->
            update (ReceivedOpponentMove move) model

        OpponentLeft ->
            update ReceivedOpponentLeft model


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    let
        t =
            translations model.language

        darkModeStyles =
            if model.darkMode then
                [ style "background-color" Color.darkBackground
                , style "color" Color.darkText
                ]

            else
                [ style "background-color" Color.lightBackground
                , style "color" Color.lightText
                ]
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
            """
            ]
        , div
            [ style "min-height" "100vh"
            , style "min-height" "100dvh"
            , style "width" "100%"
            , style "background"
                (if model.darkMode then
                    "linear-gradient(135deg, #111111 0%, #1a1f24 100%)"

                 else
                    "linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)"
                )
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
                    viewHome model t

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


viewGameButton : FrontendModel -> String -> FrontendMsg -> Html FrontendMsg
viewGameButton model label msg =
    button
        [ style "padding" "15px 20px"
        , style "font-size" "0.8em"
        , style "font-family" "inherit"
        , style "background-color"
            (if model.darkMode then
                Color.darkSecondaryBackground

             else
                Color.primary
            )
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
        ]
        [ text label ]


viewHome : FrontendModel -> Translation -> Html FrontendMsg
viewHome model t =
    let
        darkModeStyles =
            if model.darkMode then
                [ style "background-color" Color.darkBackground
                , style "color" Color.darkText
                ]

            else
                [ style "background-color" Color.lightBackground
                , style "color" Color.lightText
                ]
    in
    div
        ([ style "border-radius" "20px"
         , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
         , style "padding" "40px"
         , style "text-align" "center"
         , style "max-width" "400px"
         , style "width" "90%"
         ]
            ++ darkModeStyles
        )
        [ h1
            [ style "margin" "0 0 20px 0"
            , style "color" (Color.getText model.darkMode)
            , style "font-size" "1.5em"
            ]
            [ text t.welcome ]
        , p
            [ style "color" (Color.getText model.darkMode)
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
            [ viewGameButton model t.playWithFriend StartGameWithFriend
            , if model.botDifficultyMenuOpen then
                viewBotDifficultyMenu model

              else
                viewGameButton model t.playWithBot StartGameWithBot
            , viewGameButton model t.rulesTitle ToggleRulesModal
            , button
                [ class "menu-button"
                , onClick StartOnlineGame
                , disabled model.inMatchmaking
                , style "padding" "15px 20px"
                , style "font-size" "0.8em"
                , style "font-family" "inherit"
                , style "background-color"
                    (if model.darkMode then
                        Color.darkSecondaryBackground

                     else
                        Color.primary
                    )
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "10px"
                , style "cursor"
                    (if model.inMatchmaking then
                        "not-allowed"

                     else
                        "pointer"
                    )
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
                [ text
                    (if model.inMatchmaking then
                        t.searching

                     else
                        t.playOnline
                    )
                ]
            ]
        , if model.rulesModalVisible then
            viewRulesModal model

          else
            text ""
        ]


viewGame : FrontendModel -> GameMode -> Html FrontendMsg
viewGame model mode =
    let
        t =
            translations model.language

        darkModeStyles =
            if model.darkMode then
                [ style "background-color" Color.darkBackground
                , style "color" Color.darkText
                ]

            else
                [ style "background-color" Color.lightBackground
                , style "color" Color.lightText
                ]

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
        ([ style "display" "flex"
         , style "flex-direction" "column"
         , style "padding" "20px"
         , style "border-radius" "20px"
         , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
         , style "max-width" "800px"
         , style "width" "90%"
         , style "box-sizing" "border-box"
         , style "position" "relative"
         ]
            ++ darkModeStyles
        )
        [ div
            [ style "text-align" "center"
            , style "margin-bottom" "20px"
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
            , style "margin-bottom" "15px"
            ]
            [ viewBigBoard model ]
        , case mode of
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

            WithFriend ->
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
                    , onClick ReturnToMenu
                    ]
                    [ text t.backToMenu ]

            _ ->
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
        ]


viewBotDifficultyMenu : FrontendModel -> Html FrontendMsg
viewBotDifficultyMenu model =
    let
        t =
            translations model.language
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "10px"
        , style "width" "100%"
        ]
        [ h2
            [ style "margin" "0 0 15px 0"
            , style "color" (Color.getText model.darkMode)
            , style "font-size" "1.5em"
            ]
            [ text t.chooseDifficulty ]
        , viewDifficultyButton model t.easy Easy
        , viewDifficultyButton model t.medium Medium
        , viewDifficultyButton model t.hard Hard
        , viewDifficultyButton model t.elite Elite
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
viewDifficultyButton model label difficulty =
    button
        [ style "padding" "12px"
        , style "font-size" "0.8em"
        , style "font-family" "inherit"
        , style "background-color"
            (if model.selectedDifficulty == Just difficulty then
                Color.success

             else if model.darkMode then
                Color.darkSecondaryBackground

             else
                Color.primary
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


viewStatus : FrontendModel -> Html msg
viewStatus model =
    let
        t =
            translations model.language
    in
    div
        [ style "margin" "10px 0"
        , style "color" (Color.getText model.darkMode)
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
                                                t.playerWins (I18n.playerToString model.language player)

                                    _ ->
                                        t.playerWins (I18n.playerToString model.language player)

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
viewLanguageSelector model =
    div
        [ style "position" "absolute"
        , style "top" "20px"
        , style "right" "20px"
        , style "display" "flex"
        , style "gap" "10px"
        , style "align-items" "center"
        , style "background-color"
            (Color.withAlpha
                (if model.darkMode then
                    Color.darkBackground

                 else
                    Color.lightBackground
                )
                0.8
            )
        , style "padding" "6px"
        , style "border-radius" "10px"
        , style "backdrop-filter" "blur(10px)"
        , style "box-shadow" "0 2px 10px rgba(0, 0, 0, 0.1)"
        , style "z-index" "1000"
        ]
        [ viewDarkModeButton model.darkMode
        , div [ style "width" "1px", style "height" "20px", style "background-color" (Color.getBorder model.darkMode) ] []
        , viewLanguageButton "FR" FR (model.language == FR) model.darkMode
        , viewLanguageButton "EN" EN (model.language == EN) model.darkMode
        ]


viewDarkModeButton : Bool -> Html FrontendMsg
viewDarkModeButton isDark =
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
        , style "color"
            (if isDark then
                Color.darkTextHover

             else
                Color.lightText
            )
        , style "transition" "all 0.2s ease"
        , onClick ToggleDarkMode
        ]
        [ text
            (if isDark then
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
viewBigBoard model =
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
            , style "width" "min(70vh, 100%)"
            , style "aspect-ratio" "1/1"
            , style "margin" "0 auto"
            , style "padding" "4px"
            , style "background-color" (Color.getBorder model.darkMode)
            , style "border-radius" "12px"
            ]

        boardElements =
            List.indexedMap (viewSmallBoard model) boards

        blinkStyle =
            if shouldBlink then
                [ style "animation" "bigBoardBlink 1s ease-in-out infinite" ]

            else
                []

        opacity =
            case model.tutorialState of
                Just TutorialIntro ->
                    [ style "opacity" "0.5"
                    , style "pointer-events" "none"
                    ]

                _ ->
                    []
    in
    div
        (boardStyle ++ blinkStyle ++ opacity)
        boardElements


viewSmallBoard : FrontendModel -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard model boardIndex smallBoardData =
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
                Just TutorialIntro ->
                    False

                Just TutorialBasicMove ->
                    boardIndex == 4

                Just TutorialBoardSelection ->
                    True

                -- On montre tous les plateaux
                Just TutorialWinningSmall ->
                    boardIndex == 4

                -- Only highlight the center board
                Just TutorialWinningBig ->
                    boardIndex == 8

                -- Only highlight bottom-right board
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

                        -- Désactive les clics pendant l'étape 3
                        Just TutorialWinningSmall ->
                            boardIndex == 4

                        -- Permet de cliquer dans le morpion central
                        Just TutorialFreeChoice ->
                            False

                        -- Désactive les clics car c'est au tour de O
                        _ ->
                            isTutorialBoard
                   )

        borderColor =
            case model.tutorialState of
                Just TutorialBoardSelection ->
                    if boardIndex == 2 then
                        Color.success
                        -- Met en vidence le plateau en haut à droite

                    else
                        Color.getBorder model.darkMode

                _ ->
                    if isActive then
                        Color.success

                    else
                        Color.getBorder model.darkMode

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
                        Color.getBackground model.darkMode

        cellStyle =
            [ style "box-shadow" ("inset 0 0 0 1px " ++ Color.getBorder model.darkMode) ]

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






viewCell : FrontendModel -> Int -> Bool -> List (Html.Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewCell model boardIndex isClickable cellStyles cellIndex cellState =
    case model.tutorialState of
        Just _ ->
            Tutorial.View.viewTutorialCell model boardIndex (if isClickable then 1 else 0) cellStyles cellIndex cellState

        Nothing ->
            let
                ( symbol, textColor, bgColor ) =
                    case cellState of
                        Empty ->
                            ( Html.text ""
                            , if model.darkMode then
                                Color.darkText
                              else
                                Color.lightText
                            , if model.darkMode then
                                Color.darkBackground
                              else
                                Color.lightBackground
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
                            , if model.darkMode then
                                Color.darkBackground
                              else
                                Color.lightBackground
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
                            , if model.darkMode then
                                Color.darkBackground
                              else
                                Color.lightBackground
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
                 , style "background-color" bgColor
                 , style "display" "flex"
                 , style "align-items" "center"
                 , style "justify-content" "center"
                 , style "cursor" cursor
                 , style "color" textColor
                 , style "user-select" "none"
                 , style "position" "relative"
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




subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [ receiveLocalStorage ReceivedLocalStorage
        , receiveLocalStorageValue_
            (\jsonStr ->
                case D.decodeString localStorageValueDecoder jsonStr of
                    Ok { key, value } ->
                        ReceivedLocalStorageValue key value

                    Err _ ->
                        NoOp
            )
        , if model.isDraggingDebugger then
            Sub.batch
                [ Browser.Events.onMouseMove
                    (D.map2 DragDebugger
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                , Browser.Events.onMouseUp
                    (D.succeed StopDraggingDebugger)
                ]

          else if model.isResizingDebugger then
            Sub.batch
                [ Browser.Events.onMouseMove
                    (D.map2 ResizeDebugger
                        (D.field "clientX" D.float)
                        (D.field "clientY" D.float)
                    )
                , Browser.Events.onMouseUp
                    (D.succeed StopResizingDebugger)
                ]

          else
            Sub.none
        ]



storeLocalStorage : { key : String, value : String } -> Cmd msg
storeLocalStorage { key, value } =
    let
        json =
            E.object
                [ ( "key", E.string key )
                , ( "value", E.string value )
                ]
    in
    storeLocalStorage_ (E.encode 0 json)


receiveLocalStorage : ({ language : String, darkMode : Bool } -> msg) -> Sub msg
receiveLocalStorage toMsg =
    receiveLocalStorage_
        (\jsonStr ->
            case D.decodeString storageDecoder jsonStr of
                Ok data ->
                    toMsg data

                Err _ ->
                    toMsg { language = "FR", darkMode = False }
        )


storageDecoder : D.Decoder { language : String, darkMode : Bool }
storageDecoder =
    D.map2 (\lang dark -> { language = lang, darkMode = dark })
        (D.field "language" D.string)
        (D.field "darkMode" D.bool)


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


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
            makeMove board move.boardIndex move.cellIndex
        )
        freshBoard
        movesToApply


viewRulesModal : FrontendModel -> Html FrontendMsg
viewRulesModal model =
    let
        t =
            translations model.language
    in
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
            [ style "background-color" (Color.getBackground model.darkMode)
            , style "padding" "30px"
            , style "border-radius" "15px"
            , style "text-align" "center"
            , style "animation" "slideIn 0.3s ease-out"
            , Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True ))
            ]
            [ h2
                [ style "margin" "0 0 20px 0"
                , style "font-size" "1.2em"
                , style "color" (Color.getText model.darkMode)
                ]
                [ text t.rulesTitle ]
            , pre
                [ style "color" (Color.getText model.darkMode)
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


viewTutorialOverlay : FrontendModel -> Html FrontendMsg
viewTutorialOverlay model =
    case model.tutorialState of
        Just step ->
            let
                t =
                    translations model.language

                tutorialMessage =
                    case step of
                        TutorialIntro ->
                            t.tutorialIntro

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
                        TutorialIntro ->
                            1

                        TutorialBasicMove ->
                            2

                        TutorialBoardSelection ->
                            3

                        TutorialWinningSmall ->
                            4

                        TutorialFreeChoice ->
                            5

                        TutorialWinningBig ->
                            6

                overlayStyles =
                    case step of
                        TutorialIntro ->
                            [ style "top" "50%"
                            , style "transform" "translate(-50%, -50%)"
                            ]

                        _ ->
                            [ style "bottom" "20px"
                            , style "transform" "translateX(-50%)"
                            ]
            in
            div
                ([ style "position" "fixed"
                 , style "left" "50%"
                 , style "background-color" (Color.getBackground model.darkMode)
                 , style "padding" "20px"
                 , style "border-radius" "15px"
                 , style "box-shadow" "0 4px 12px rgba(0, 0, 0, 0.2)"
                 , style "max-width" "90%"
                 , style "width" "600px"
                 , style "text-align" "center"
                 , style "animation" "slideIn 0.3s ease-out"
                 , style "z-index" "1000"
                 ]
                    ++ overlayStyles
                )
                [ div
                    [ style "margin-bottom" "10px"
                    , style "color" (Color.getText model.darkMode)
                    , style "font-size" "0.8em"
                    ]
                    [ text (String.fromInt stepNumber ++ "/6") ]
                , p
                    [ style "margin" "0 0 15px 0"
                    , style "color" (Color.getText model.darkMode)
                    , style "font-size" "1em"
                    , style "line-height" "1.5"
                    ]
                    [ text tutorialMessage ]
                , div
                    [ style "display" "flex"
                    , style "gap" "10px"
                    , style "justify-content" "center"
                    ]
                    [ button
                        [ style "padding" "10px 20px"
                        , style "font-size" "0.9em"
                        , style "background-color" Color.primary
                        , style "color" "white"
                        , style "border" "none"
                        , style "border-radius" "6px"
                        , style "cursor" "pointer"
                        , style "transition" "all 0.2s ease"
                        , onClick SkipTutorial
                        ]
                        [ text t.skipTutorial ]
                    , button
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
                ]

        _ ->
            text ""


isBigBoardComplete : BigBoard -> Bool
isBigBoardComplete board =
    List.all isSmallBoardComplete board.boards


viewGameResultModal : FrontendModel -> Html FrontendMsg
viewGameResultModal model =
    let
        t =
            translations model.language

        resultText =
            case model.gameResult of
                Won ->
                    t.youWon

                Lost ->
                    t.youLost

                Drew ->
                    t.draw

                Ongoing ->
                    ""
    in
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
            [ style "background-color" (Color.getBackground model.darkMode)
            , style "padding" "30px"
            , style "border-radius" "15px"
            , style "text-align" "center"
            , style "animation" "slideIn 0.3s ease-out"
            ]
            [ h2
                [ style "margin" "0 0 20px 0"
                , style "font-size" "1.2em"
                , style "color" (Color.getText model.darkMode)
                ]
                [ text resultText ]
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
