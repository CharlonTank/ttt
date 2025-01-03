module Bot exposing (findBestMove)

import GameLogic exposing (makeMove, nextPlayer)
import List.Extra as List
import Types exposing (..)


findBestMove : FrontendGame -> BotDifficulty -> Maybe Move
findBestMove game difficulty =
    let
        availableMoves =
            getAllAvailableMoves game

        isFirstMove =
            List.all
                (\smallBoard ->
                    List.all ((==) Empty) smallBoard.cells
                )
                game.boards

        depthForDifficulty =
            case difficulty of
                Easy ->
                    2

                Medium ->
                    3

                Hard ->
                    4

                Elite ->
                    if isFirstMove then
                        4

                    else
                        5

        moveScores : List ( Move, Int )
        moveScores =
            List.map
                (\({ boardIndex, cellIndex } as move) ->
                    let
                        newBoard =
                            makeMove game move game.currentPlayer

                        baseScore =
                            alphabeta newBoard depthForDifficulty -10000 10000 False

                        -- Add randomness based on difficulty
                        randomFactor =
                            case difficulty of
                                Easy ->
                                    modBy 200 (boardIndex * 17 + cellIndex * 13)

                                Medium ->
                                    modBy 100 (boardIndex * 11 + cellIndex * 7)

                                Hard ->
                                    modBy 50 (boardIndex * 7 + cellIndex * 5)

                                Elite ->
                                    modBy 10 (boardIndex * 3 + cellIndex * 2)

                        -- Petit facteur aléatoire pour Elite
                    in
                    ( move, baseScore + randomFactor )
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


alphabeta : FrontendGame -> Int -> Int -> Int -> Bool -> Int
alphabeta game depth alpha beta isMaximizing =
    case game.winner of
        Just winner ->
            if winner == game.currentPlayer then
                1000 + depth

            else
                -1000 - depth

        Nothing ->
            if depth == 0 then
                evaluatePosition game game.currentPlayer

            else if isMaximizing then
                alphabetaMax game depth alpha beta

            else
                alphabetaMin game depth alpha beta


alphabetaMax : FrontendGame -> Int -> Int -> Int -> Int
alphabetaMax game depth alpha beta =
    let
        availableMoves =
            getAllAvailableMoves game

        helper moves currentAlpha bestScore =
            case moves of
                [] ->
                    bestScore

                move :: rest ->
                    let
                        newBoard =
                            makeMove game move game.currentPlayer

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


alphabetaMin : FrontendGame -> Int -> Int -> Int -> Int
alphabetaMin game depth alpha beta =
    let
        helper moves currentBeta bestScore =
            case moves of
                [] ->
                    bestScore

                move :: rest ->
                    let
                        newBoard =
                            makeMove game move game.currentPlayer

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
    helper (getAllAvailableMoves game) beta 10000


evaluatePosition : FrontendGame -> Player -> Int
evaluatePosition game forPlayer =
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
            List.map evaluateSmallBoard game.boards

        -- Add bonus for center board control
        centerBoardBonus =
            case List.getAt 4 game.boards of
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
                    List.filterMap (\i -> List.getAt i game.boards) cornerIndexes

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
            case game.activeBoard of
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


getAllAvailableMoves : FrontendGame -> List Move
getAllAvailableMoves game =
    let
        validBoardIndexes =
            case game.activeBoard of
                Nothing ->
                    List.range 0 8

                Just idx ->
                    [ idx ]

        isValidBoardAndCell move =
            isValidMove game move
    in
    List.concatMap
        (\boardIdx -> List.map (\cellIdx -> { boardIndex = boardIdx, cellIndex = cellIdx }) (List.range 0 8))
        validBoardIndexes
        |> List.filter isValidBoardAndCell


isValidMove : FrontendGame -> Move -> Bool
isValidMove game move =
    case game.winner of
        Just _ ->
            False

        Nothing ->
            let
                targetBoard =
                    List.drop move.boardIndex game.boards |> List.head |> Maybe.withDefault emptySmallBoard

                targetCell =
                    List.drop move.cellIndex targetBoard.cells |> List.head |> Maybe.withDefault Empty
            in
            targetBoard.winner == Nothing && targetCell == Empty


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }
