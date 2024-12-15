module Bot exposing (findBestMove)

import Types exposing (..)
import List.Extra as List


findBestMove : BigBoard -> BotDifficulty -> Maybe (Int, Int)
findBestMove board difficulty =
    let
        availableMoves =
            getAllAvailableMoves board

        isFirstMove =
            List.all (\smallBoard -> 
                List.all ((==) Empty) smallBoard.cells
            ) board.boards

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


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
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