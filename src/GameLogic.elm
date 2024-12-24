module GameLogic exposing
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

import List.Extra as List
import Types exposing (BigBoard, CellState(..), Player(..), SmallBoard)


initialBoard : Player -> BigBoard
initialBoard startingPlayer =
    { boards = List.repeat 9 emptySmallBoard
    , currentPlayer = startingPlayer
    , activeBoard = Nothing
    , winner = Nothing
    , initialPlayer = startingPlayer
    , lastMove = Nothing
    }


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }


isValidMove : BigBoard -> Int -> Int -> Bool
isValidMove board boardIndex cellIndex =
    case board.winner of
        Just _ ->
            False

        Nothing ->
            let
                targetBoard =
                    List.getAt boardIndex board.boards |> Maybe.withDefault emptySmallBoard

                targetCell =
                    List.getAt cellIndex targetBoard.cells |> Maybe.withDefault Empty
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
            if isSmallBoardComplete (List.getAt cellIndex updatedBoards |> Maybe.withDefault emptySmallBoard) then
                Nothing

            else
                Just cellIndex
    in
    { board
        | boards = updatedBoards
        , currentPlayer = nextPlayer
        , activeBoard = nextActiveBoard
        , winner = checkBigBoardWinner updatedBoards
        , lastMove = Just { boardIndex = boardIndex, cellIndex = cellIndex, player = player }
    }


isSmallBoardComplete : SmallBoard -> Bool
isSmallBoardComplete board =
    board.winner /= Nothing || List.all ((/=) Empty) board.cells


isBigBoardComplete : BigBoard -> Bool
isBigBoardComplete board =
    List.all isSmallBoardComplete board.boards


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
            List.getAt index boardWinners |> Maybe.withDefault Nothing

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
