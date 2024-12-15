module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n exposing (Translation, translations)
import Bot
import Lamdera
import List.Extra as List
import Types exposing (..)
import Url
import Task
import Process
import Debug


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
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
      }
    , Cmd.none
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
                                        handlePlayerMove model boardIdx cellIdx

                                    Nothing ->
                                        ( model, Cmd.none )
                        in
                        ( { newModel | botThinking = False }, cmd )
                    else
                        ( model, Cmd.none )

                Game WithFriend ->
                    ( model, Cmd.none )

                Home ->
                    ( model, Cmd.none )

        RestartGame ->
            ( { model | board = initialBoard }, Cmd.none )

        StartGameWithFriend ->
            ( { model | route = Game WithFriend }, Cmd.none )

        StartGameWithBot ->
            ( { model | botDifficultyMenuOpen = True }, Cmd.none )

        SelectBotDifficulty difficulty ->
            ( { model 
              | route = Game (WithBot difficulty)
              , botDifficultyMenuOpen = False
              }
            , Cmd.none
            )

        ReturnToMenu ->
            ( { model | route = Home }, Cmd.none )

        CancelBotDifficulty ->
            ( { model | botDifficultyMenuOpen = False }, Cmd.none )

        ChangeLanguage lang ->
            ( { model | language = lang }, Cmd.none )


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
                { model | board = makeMove model.board boardIndex cellIndex }
            else
                model
    in
    ( updatedModel, Cmd.none )


findBestMove : BigBoard -> BotDifficulty -> Maybe (Int, Int)
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
                                    modBy 10 (boardIdx * 3 + cellIdx * 2)  -- Petit facteur aléatoire pour Elite
                    in
                    ( ( boardIdx, cellIdx ), baseScore + randomFactor )
                )
                availableMoves

        -- For Easy and Medium, sometimes choose a random move
        shouldChooseRandom =
            case difficulty of
                Easy ->
                    modBy 3 (List.length availableMoves) == 0  -- 33% chance
                Medium ->
                    modBy 5 (List.length availableMoves) == 0  -- 20% chance
                Hard ->
                    modBy 10 (List.length availableMoves) == 0  -- 10% chance
                Elite ->
                    modBy 20 (List.length availableMoves) == 0  -- 5% chance

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
                1000 + depth  -- Bot wins (O)
            else
                -1000 - depth  -- Player wins (X)

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
                            List.count (\cell -> 
                                case cell of
                                    Filled p -> p /= forPlayer
                                    Empty -> False
                            ) line

                        emptyCount =
                            List.count ((==) Empty) line
                    in
                    if playerCount == 3 then
                        100  -- Winning line
                    else if opponentCount == 3 then
                        -100  -- Opponent winning line
                    else if playerCount == 2 && emptyCount == 1 then
                        20   -- Two in a row with potential
                    else if opponentCount == 2 && emptyCount == 1 then
                        -20  -- Block opponent's potential win
                    else if playerCount == 1 && emptyCount == 2 then
                        2    -- One with potential
                    else if opponentCount == 1 && emptyCount == 2 then
                        -2   -- Opponent one with potential
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
                                200  -- Increased importance of center
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
                                100  -- Increased importance of corners
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
                        -50  -- Penalize sending opponent to center
                    else if List.member nextBoardIndex [ 0, 2, 6, 8 ] then
                        -30  -- Penalize sending opponent to corners
                    else
                        20   -- Bonus for sending opponent to less strategic boards

                Nothing ->
                    0
    in
    List.sum boardScores + centerBoardBonus + cornerBoardsBonus + strategicBonus


getAllAvailableMoves : BigBoard -> List (Int, Int)
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
    in
    { title = t.welcome
    , body =
        [ div 
            [ style "min-height" "100vh"
            , style "min-height" "100dvh"
            , style "width" "100%"
            , style "background" "linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "padding" "env(safe-area-inset-top, 10px) env(safe-area-inset-right, 10px) env(safe-area-inset-bottom, 10px) env(safe-area-inset-left, 10px)"
            , style "box-sizing" "border-box"
            , style "position" "relative"
            ]
            [ Html.node "style" 
                [] 
                [ text """
                    @keyframes thinking {
                        0%, 100% { opacity: 0.3; transform: scale(0.8); }
                        50% { opacity: 1; transform: scale(1.2); }
                    }
                """
                ]
            , viewLanguageSelector model.language
            , case model.route of
                Home ->
                    viewHome model
                
                Game mode ->
                    viewGame model mode
            ]
        ]
    }


viewGameButton : String -> FrontendMsg -> Html FrontendMsg
viewGameButton label msg =
    button
        [ style "padding" "15px 40px"
        , style "font-size" "1.2em"
        , style "font-weight" "600"
        , style "background-color" "#3498db"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "10px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "box-shadow" "0 4px 6px rgba(52, 152, 219, 0.2)"
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
        [ style "background-color" "white"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "padding" "40px"
        , style "text-align" "center"
        , style "max-width" "400px"
        , style "width" "90%"
        ]
        [ h1 
            [ style "margin" "0 0 20px 0"
            , style "color" "#2c3e50"
            , style "font-size" "2.5em"
            ] 
            [ text t.welcome ]
        , p 
            [ style "color" "#34495e"
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
            [ viewGameButton t.playWithFriend StartGameWithFriend
            , if model.botDifficultyMenuOpen then
                viewBotDifficultyMenu model
              else
                viewGameButton t.playWithBot StartGameWithBot
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
            , style "color" "#2c3e50"
            , style "font-size" "1.5em"
            ] 
            [ text t.chooseDifficulty ]
        , viewDifficultyButton t.easy Easy
        , viewDifficultyButton t.medium Medium
        , viewDifficultyButton t.hard Hard
        , viewDifficultyButton t.elite Elite
        , button
            [ style "padding" "12px"
            , style "font-size" "1.1em"
            , style "font-weight" "600"
            , style "background-color" "#e74c3c"
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


viewDifficultyButton : String -> BotDifficulty -> Html FrontendMsg
viewDifficultyButton label difficulty =
    button
        [ style "padding" "12px"
        , style "font-size" "1.1em"
        , style "font-weight" "600"
        , style "background-color" "#3498db"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "8px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "width" "100%"
        , onClick (SelectBotDifficulty difficulty)
        ]
        [ text label ]


viewGame : Model -> GameMode -> Html FrontendMsg
viewGame model mode =
    let
        t =
            translations model.language
    in
    div 
        [ style "background-color" "white"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "width" "min(95vw, 90vh)"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "box-sizing" "border-box"
        , style "overflow" "hidden"
        , style "padding" "20px"
        ]
        [ div 
            [ style "text-align" "center"
            , style "margin-bottom" "15px"
            ]
            [ h1 
                [ style "margin" "0"
                , style "color" "#2c3e50"
                , style "font-size" "clamp(1.2em, 3vmin, 2em)"
                , style "font-weight" "700"
                ] 
                [ text t.welcome ]
            , div
                [ style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "gap" "10px"
                , style "margin-top" "10px"
                , style "color" "#34495e"
                , style "font-size" "0.9em"
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
            [ viewBigBoard model.board ]
        , div
            [ style "display" "flex"
            , style "gap" "10px"
            ]
            [ button
                [ style "width" "100%"
                , style "padding" "12px"
                , style "font-size" "clamp(12px, 2vmin, 16px)"
                , style "font-weight" "600"
                , style "background-color" "#3498db"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "10px"
                , style "cursor" "pointer"
                , style "transition" "all 0.2s ease"
                , onClick RestartGame
                ]
                [ text t.restart ]
            , button
                [ style "width" "100%"
                , style "padding" "12px"
                , style "font-size" "clamp(12px, 2vmin, 16px)"
                , style "font-weight" "600"
                , style "background-color" "#e74c3c"
                , style "color" "white"
                , style "border" "none"
                , style "border-radius" "10px"
                , style "cursor" "pointer"
                , style "transition" "all 0.2s ease"
                , onClick ReturnToMenu
                ]
                [ text t.backToMenu ]
            ]
        ]


viewStatus : Model -> Html msg
viewStatus model =
    let
        t =
            translations model.language
    in
    div 
        [ style "margin" "10px 0"
        , style "color" "#34495e"
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


viewLanguageSelector : Language -> Html FrontendMsg
viewLanguageSelector currentLang =
    div
        [ style "position" "absolute"
        , style "top" "20px"
        , style "right" "20px"
        , style "display" "flex"
        , style "gap" "10px"
        ]
        [ viewLanguageButton "FR" FR (currentLang == FR)
        , viewLanguageButton "EN" EN (currentLang == EN)
        ]


viewLanguageButton : String -> Language -> Bool -> Html FrontendMsg
viewLanguageButton label lang isActive =
    button
        [ style "padding" "8px 12px"
        , style "font-size" "0.9em"
        , style "font-weight" "600"
        , style "background-color" (if isActive then "#3498db" else "#e2e8f0")
        , style "color" (if isActive then "white" else "#2c3e50")
        , style "border" "none"
        , style "border-radius" "6px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , onClick (ChangeLanguage lang)
        ]
        [ text label ]


viewBigBoard : BigBoard -> Html FrontendMsg
viewBigBoard board =
    div 
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "gap" "10px"
        , style "width" "min(70vh, 100%)"
        , style "aspect-ratio" "1/1"
        , style "margin" "0 auto"
        , style "padding" "2px"
        ]
        (List.indexedMap (viewSmallBoard board) board.boards)


viewSmallBoard : BigBoard -> Int -> SmallBoard -> Html FrontendMsg
viewSmallBoard bigBoard boardIndex smallBoard =
    let
        isActive =
            case bigBoard.activeBoard of
                Nothing ->
                    True

                Just activeBoardIndex ->
                    activeBoardIndex == boardIndex

        borderColor =
            if isActive then
                "#4CAF50"
            else
                "#e2e8f0"

        backgroundColor =
            case smallBoard.winner of
                Just X ->
                    "#ff8a65"

                Just O ->
                    "#64b5f6"

                Nothing ->
                    "#ffffff"
    in
    div
        [ style "background-color" backgroundColor
        , style "border-radius" "8px"
        , style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "gap" "2px"
        , style "padding" "4px"
        , style "aspect-ratio" "1/1"
        , style "box-shadow" ("0 0 0 2px " ++ borderColor)
        ]
        (List.indexedMap (viewCell bigBoard boardIndex) smallBoard.cells)


viewCell : BigBoard -> Int -> Int -> CellState -> Html FrontendMsg
viewCell board boardIndex cellIndex cellState =
    let
        symbol =
            case cellState of
                Empty ->
                    ""

                Filled X ->
                    "×"

                Filled O ->
                    "○"

        (textColor, hoverBg) =
            case cellState of
                Empty ->
                    ("#000000", "#f1f5f9")

                Filled X ->
                    ("#e74c3c", "")

                Filled O ->
                    ("#3498db", "")
    in
    div
        [ style "width" "100%"
        , style "aspect-ratio" "1/1"
        , style "box-shadow" "0 0 0 1px #e2e8f0"
        , style "background-color" "#ffffff"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "clamp(2em, 8vmin, 4em)"
        , style "font-weight" "bold"
        , style "cursor" (if cellState == Empty then "pointer" else "default")
        , style "color" textColor
        , style "user-select" "none"
        , style "border-radius" "4px"
        , onClick (CellClicked boardIndex cellIndex)
        ]
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