module Frontend exposing (..)

import Bot
import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Color
import Debug
import Debugger
import Dict
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
import Task
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


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


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , board = initialBoard
      , route = Home
      , botDifficultyMenuOpen = False
      , language = FR
      , botThinking = False
      , moveHistory = []
      , currentMoveIndex = -1
      , darkMode = False
      , humanPlaysFirst = True
      , frClickCount = 0
      , debuggerVisible = False
      , debuggerPosition = { x = 20, y = 20 }
      , isDraggingDebugger = False
      , dragOffset = { x = 0, y = 0 }
      , debuggerSize = { width = 400, height = 300 }
      , isResizingDebugger = False
      , localStorageValues = Dict.empty
      , selectedDifficulty = Nothing
      , rulesModalVisible = False
      }
    , Cmd.batch
        [ getLocalStorageValue_ "language"
        , getLocalStorageValue_ "darkMode"
        ]
    )


initialBoard : BigBoard
initialBoard =
    { boards = List.repeat 9 emptySmallBoard
    , currentPlayer = X
    , activeBoard = Nothing
    , winner = Nothing
    }


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
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
                                        in
                                        ( { modelAfterMove | botThinking = False }, moveCmd )

                                    Nothing ->
                                        ( { model | botThinking = False }, Cmd.none )
                        in
                        ( newModel, cmd )

                    else
                        ( model, Cmd.none )

                Game WithFriend ->
                    ( model, Cmd.none )

                Home ->
                    ( model, Cmd.none )

        RestartGame ->
            ( { model
                | board = initialBoard
                , moveHistory = []
                , currentMoveIndex = -1
              }
            , Cmd.none
            )

        StartGameWithFriend ->
            ( { model | route = Game WithFriend }, Cmd.none )

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
                newBoard =
                    { initialBoard
                        | currentPlayer =
                            if humanStarts then
                                X

                            else
                                O
                    }

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
                              }
                            , cmd
                            )

                        Nothing ->
                            ( model, Cmd.none )

        ReturnToMenu ->
            ( { model | route = Home }, Cmd.none )

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
                        reconstructBoardFromMoves model.moveHistory newIndex
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
                        reconstructBoardFromMoves model.moveHistory newIndex
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


handlePlayerMove : Model -> Int -> Int -> ( Model, Cmd FrontendMsg )
handlePlayerMove model boardIndex cellIndex =
    let
        canPlayInBoard =
            case model.board.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    activeBoardIndex == boardIndex

        updatedModel =
            if canPlayInBoard && isValidMove model.board boardIndex cellIndex then
                let
                    newBoard =
                        makeMove model.board boardIndex cellIndex

                    -- Create new move record
                    newMove =
                        { boardIndex = boardIndex
                        , cellIndex = cellIndex
                        , player = model.board.currentPlayer
                        }

                    -- Truncate future history if we're not at the end
                    newHistory =
                        List.take (model.currentMoveIndex + 1) model.moveHistory ++ [ newMove ]

                    newIndex =
                        model.currentMoveIndex + 1
                in
                { model
                    | board = newBoard
                    , moveHistory = newHistory
                    , currentMoveIndex = newIndex
                }

            else
                model
    in
    ( updatedModel, Cmd.none )


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
                            List.count (\cell -> cell == Filled forPlayer) line

                        opponentCount =
                            List.count
                                (\cell ->
                                    case cell of
                                        Filled p ->
                                            p /= forPlayer

                                        Empty ->
                                            False
                                )
                                line

                        emptyCount =
                            List.count ((==) Empty) line
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

        getCellState index =
            List.drop index cells |> List.head |> Maybe.withDefault Empty

        checkCombination indexes =
            case List.map getCellState indexes of
                (Filled player) :: rest ->
                    if List.all ((==) (Filled player)) rest then
                        Just player

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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
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
        [ div
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
            ]
            ([ Html.node "style"
                []
                [ text """
                    @keyframes thinking {
                        0%, 100% { opacity: 0.3; transform: scale(0.8); }
                        50% { opacity: 1; transform: scale(1.2); }
                    }
                    @keyframes blink {
                        0% { opacity: 0.5; }
                        50% { opacity: 1; }
                        100% { opacity: 0.5; }
                    }
                """
                ]
             , viewLanguageSelector model
             , case model.route of
                Home ->
                    viewHome model

                Game mode ->
                    viewGame model mode
             ]
                ++ Debugger.view model
            )
        ]
    }


viewGameButton : Model -> String -> FrontendMsg -> Html FrontendMsg
viewGameButton model label msg =
    button
        [ style "padding" "15px 40px"
        , style "font-size" "1.2em"
        , style "font-weight" "600"
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


viewHome : Model -> Html FrontendMsg
viewHome model =
    let
        t =
            translations model.language
    in
    div
        [ style "background-color" (Color.getBackground model.darkMode)
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "padding" "40px"
        , style "text-align" "center"
        , style "max-width" "400px"
        , style "width" "90%"
        ]
        [ h1
            [ style "margin" "0 0 20px 0"
            , style "color" (Color.getText model.darkMode)
            , style "font-size" "2.5em"
            ]
            [ text t.welcome ]
        , p
            [ style "color" (Color.getText model.darkMode)
            , style "margin-bottom" "30px"
            , style "line-height" "1.6"
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
            , viewGameButton model t.rules ToggleRulesModal
            ]
        , if model.rulesModalVisible then
            viewRulesModal model

          else
            text ""
        ]


viewGame : Model -> GameMode -> Html FrontendMsg
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

        isGameStarted =
            not (List.isEmpty model.moveHistory)

        shouldShowStartingPlayer =
            case mode of
                WithBot _ ->
                    True

                _ ->
                    False

        activeBoardStyle =
            case model.board.activeBoard of
                Just activeBoardIndex ->
                    if model.board.currentPlayer == O && model.botThinking then
                        [ style "animation" "thinking 1s infinite" ]

                    else
                        []

                Nothing ->
                    []

        showPlayForMeButton =
            case mode of
                WithBot _ ->
                    model.board.currentPlayer == X && model.board.winner == Nothing

                _ ->
                    False
    in
    div
        ([ style "border-radius" "20px"
         , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
         , style "padding" "20px"
         , style "background-color" (Color.getBackground model.darkMode)
         , style "display" "flex"
         , style "flex-direction" "column"
         , style "gap" "15px"
         , style "width" "min(95vw, 600px)"
         , style "box-sizing" "border-box"
         ]
            ++ darkModeStyles
        )
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "5px"
            ]
            [ div
                [ style "font-weight" "600"
                , style "font-size" "1.2em"
                ]
                [ text <|
                    case mode of
                        WithFriend ->
                            t.playingWithFriend

                        WithBot difficulty ->
                            t.playingWithBot <|
                                case difficulty of
                                    Easy ->
                                        t.easy

                                    Medium ->
                                        t.medium

                                    Hard ->
                                        t.hard

                                    Elite ->
                                        t.elite
                ]
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
        , div
            [ style "display" "flex"
            , style "gap" "10px"
            ]
            [ button
                [ style "flex" "1"
                , style "padding" "8px"
                , style "font-size" "1.2em"
                , style "background-color"
                    (if model.currentMoveIndex > 0 then
                        Color.primary

                     else
                        Color.disabled
                    )
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "6px"
                , style "cursor"
                    (if model.currentMoveIndex > 0 then
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
        , div
            [ style "display" "flex"
            , style "gap" "10px"
            , style "flex-wrap" "wrap"
            , style "margin-top" "10px"
            ]
            [ button
                [ style "flex" "1"
                , style "padding" "12px"
                , style "font-size" "clamp(12px, 2vmin, 16px)"
                , style "font-weight" "600"
                , style "background-color" Color.danger
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "10px"
                , style "cursor" "pointer"
                , style "transition" "all 0.2s ease"
                , onClick ReturnToMenu
                ]
                [ text t.backToMenu ]
            , if showPlayForMeButton then
                button
                    [ style "flex" "1"
                    , style "padding" "12px"
                    , style "font-size" "clamp(12px, 2vmin, 16px)"
                    , style "font-weight" "600"
                    , style "background-color" Color.primary
                    , style "color" "white"
                    , style "border" "none"
                    , style "border-radius" "10px"
                    , style "cursor" "pointer"
                    , style "transition" "all 0.2s ease"
                    , onClick PlayForMe
                    ]
                    [ text t.playForMe ]

              else
                text ""
            ]
        ]


viewBotDifficultyMenu : Model -> Html FrontendMsg
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
                        , onClick (StartWithPlayer False)
                        ]
                        [ text t.botStarts ]
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


viewDifficultyButton : Model -> String -> BotDifficulty -> Html FrontendMsg
viewDifficultyButton model label difficulty =
    button
        [ style "padding" "12px"
        , style "font-size" "1.1em"
        , style "font-weight" "600"
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


viewStatus : Model -> Html msg
viewStatus model =
    let
        t =
            translations model.language
    in
    div
        [ style "margin" "10px 0"
        , style "color" (Color.getText model.darkMode)
        , style "font-size" "clamp(0.9em, 2.5vmin, 1.2em)"
        , style "font-weight" "600"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "gap" "10px"
            ]
            [ text <|
                case model.board.winner of
                    Just player ->
                        t.playerWins (I18n.playerToString model.language player)

                    Nothing ->
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
        [ div [ style "animation" "thinking 1s infinite" ] [ text "•" ]
        , div [ style "animation" "thinking 1s infinite 0.3s" ] [ text "•" ]
        , div [ style "animation" "thinking 1s infinite 0.6s" ] [ text "•" ]
        ]


viewLanguageSelector : Model -> Html FrontendMsg
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
        , style "font-size" "0.9em"
        , style "font-weight" "600"
        , style "background-color"
            (if isActive then
                Color.primary

             else
                if isDark then
                    Color.darkSecondaryBackground

                else
                    Color.lightBorder
            )
        , style "color"
            (if isActive then
                "white"

             else
                if isDark then
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


viewBigBoard : Model -> Html FrontendMsg
viewBigBoard model =
    let
        isBotTurn =
            case model.route of
                Game (WithBot _) ->
                    model.board.currentPlayer == O && model.botThinking

                _ ->
                    False

        shouldBlink =
            isBotTurn && model.board.activeBoard == Nothing
    in
    div
        ([ style "display" "grid"
         , style "grid-template-columns" "repeat(3, 1fr)"
         , style "gap" "10px"
         , style "width" "min(70vh, 100%)"
         , style "aspect-ratio" "1/1"
         , style "margin" "0 auto"
         , style "padding" "2px"
         ]
            ++ (if shouldBlink then
                    [ style "animation" "blink 1s ease-in-out infinite" ]

                else
                    []
               )
        )
        (List.indexedMap (viewSmallBoard model) model.board.boards)


viewSmallBoard : Model -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard model boardIndex smallBoard =
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

        isClickable =
            isActive && not isBotTurn

        borderColor =
            if isActive then
                Color.success

            else
                Color.getBorder model.darkMode

        backgroundColor =
            case smallBoard.winner of
                Just X ->
                    Color.playerX

                Just O ->
                    Color.playerO

                Nothing ->
                    Color.getBackground model.darkMode

        cellStyle =
            [ style "box-shadow" ("inset 0 0 0 1px " ++ Color.getBorder model.darkMode) ]

        shouldBlink =
            isActive && isBotTurn && model.board.activeBoard /= Nothing
    in
    div
        ([ style "background-color" backgroundColor
         , style "border-radius" "8px"
         , style "display" "grid"
         , style "grid-template-columns" "repeat(3, 1fr)"
         , style "gap" "2px"
         , style "padding" "4px"
         , style "aspect-ratio" "1/1"
         , style "box-shadow" ("0 0 0 2px " ++ borderColor)
         ]
            ++ (if shouldBlink then
                    [ style "animation" "blink 1s ease-in-out infinite" ]

                else
                    []
               )
        )
        (List.indexedMap (viewCell model boardIndex isClickable cellStyle) smallBoard.cells)


viewCell : Model -> Int -> Bool -> List (Html.Attribute FrontendMsg) -> Int -> CellState -> Html FrontendMsg
viewCell model boardIndex isClickable cellStyles cellIndex cellState =
    let
        symbol =
            case cellState of
                Empty ->
                    ""

                Filled X ->
                    "×"

                Filled O ->
                    "○"

        ( textColor, bgColor ) =
            case cellState of
                Empty ->
                    if model.darkMode then
                        ( Color.darkText, Color.darkBackground )

                    else
                        ( Color.lightText, Color.lightBackground )

                Filled X ->
                    ( Color.danger
                    , if model.darkMode then
                        Color.darkBackground

                      else
                        Color.lightBackground
                    )

                Filled O ->
                    ( Color.primary
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
    in
    div
        ([ style "width" "100%"
         , style "aspect-ratio" "1/1"
         , style "background-color" bgColor
         , style "display" "flex"
         , style "align-items" "center"
         , style "justify-content" "center"
         , style "font-size" "clamp(2em, 8vmin, 4em)"
         , style "font-weight" "bold"
         , style "cursor" cursor
         , style "color" textColor
         , style "user-select" "none"
         , style "border-radius" "4px"
         ]
            ++ cellStyles
            ++ (if isCellClickable then
                    [ onClick (CellClicked boardIndex cellIndex) ]

                else
                    [ style "pointer-events" "none" ]
               )
        )
        [ div
            [ style "width" "100%"
            , style "height" "100%"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "line-height" "0"
            ]
            [ text symbol ]
        ]


subscriptions : Model -> Sub FrontendMsg
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


reconstructBoardFromMoves : List Move -> Int -> BigBoard
reconstructBoardFromMoves moves upToIndex =
    let
        movesToApply =
            List.take (upToIndex + 1) moves
    in
    List.foldl
        (\move board ->
            makeMove board move.boardIndex move.cellIndex
        )
        initialBoard
        movesToApply


viewRulesModal : Model -> Html FrontendMsg
viewRulesModal model =
    let
        t =
            translations model.language

        stopPropagation msg =
            Html.Events.stopPropagationOn "click" (D.succeed ( msg, True ))
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
            , style "border-radius" "20px"
            , style "padding" "30px"
            , style "max-width" "600px"
            , style "width" "90%"
            , style "max-height" "90vh"
            , style "overflow-y" "auto"
            , style "position" "relative"
            , stopPropagation NoOp
            ]
            [ h2
                [ style "margin" "0 0 20px 0"
                , style "color" (Color.getText model.darkMode)
                , style "font-size" "1.8em"
                ]
                [ text t.rulesTitle ]
            , pre
                [ style "color" (Color.getText model.darkMode)
                , style "white-space" "pre-wrap"
                , style "font-family" "inherit"
                , style "text-align" "left"
                , style "line-height" "1.6"
                , style "margin" "0 0 20px 0"
                ]
                [ text t.rulesText ]
            , button
                [ style "padding" "12px 30px"
                , style "font-size" "1.1em"
                , style "font-weight" "600"
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
