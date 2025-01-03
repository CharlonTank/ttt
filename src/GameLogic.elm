module GameLogic exposing
    ( checkBigBoardWinner
    , checkSmallBoardWinner
    , emptySmallBoard
    , getAllAvailableMoves
    , isBigBoardComplete
    , isSmallBoardComplete
    , isValidMove
    , makeMove
    , nextPlayer
    )

import List.Extra as List
import Types exposing (CellState(..), FrontendGame, GameResult(..), Move, OnlineGame, Player(..), SmallBoard)


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }


isValidMove : OnlineGame -> Int -> Int -> Bool
isValidMove game boardIndex cellIndex =
    case game.winner of
        Just _ ->
            False

        Nothing ->
            let
                targetBoard =
                    List.getAt boardIndex game.boards |> Maybe.withDefault emptySmallBoard

                targetCell =
                    List.getAt cellIndex targetBoard.cells |> Maybe.withDefault Empty
            in
            targetBoard.winner == Nothing && targetCell == Empty


makeMove : FrontendGame -> Move -> Player -> FrontendGame
makeMove game move currentPlayer =
    if game.winner /= Nothing || game.gameResult == Just Draw then
        game

    else
        let
            updateCell cells index =
                List.indexedMap
                    (\i cell ->
                        if i == index then
                            Filled currentPlayer

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
                                    updateCell smallBoard.cells move.cellIndex
                            in
                            { smallBoard
                                | cells = updatedCells
                                , winner = checkSmallBoardWinner updatedCells
                            }

                        else
                            smallBoard
                    )
                    boards

            updatedBoards =
                updateBoard game.boards move.boardIndex
        in
        { game
            | currentPlayer = nextPlayer currentPlayer
            , lastMove = Just move
            , winner = checkBigBoardWinner updatedBoards
            , gameResult =
                if isBigBoardComplete updatedBoards then
                    Just Draw

                else
                    Nothing
            , activeBoard =
                if isSmallBoardComplete (List.getAt move.cellIndex updatedBoards |> Maybe.withDefault emptySmallBoard) then
                    Nothing

                else
                    Just move.cellIndex
            , boards = updatedBoards
        }


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X


isSmallBoardComplete : SmallBoard -> Bool
isSmallBoardComplete board =
    board.winner /= Nothing || List.all ((/=) Empty) board.cells


isBigBoardComplete : List SmallBoard -> Bool
isBigBoardComplete boards =
    List.all isSmallBoardComplete boards


winningCombinations : List (List Int)
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


checkSmallBoardWinner : List CellState -> Maybe Player
checkSmallBoardWinner cells =
    List.filterMap
        (\indices ->
            case List.map (\i -> List.getAt i cells) indices of
                [ Just (Filled p1), Just (Filled p2), Just (Filled p3) ] ->
                    if p1 == p2 && p2 == p3 then
                        Just p1

                    else
                        Nothing

                _ ->
                    Nothing
        )
        winningCombinations
        |> List.head


checkBigBoardWinner : List SmallBoard -> Maybe Player
checkBigBoardWinner boards =
    let
        getWinner index =
            List.getAt index (List.map .winner boards) |> Maybe.withDefault Nothing

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


getAllAvailableMoves : OnlineGame -> List ( Int, Int )
getAllAvailableMoves game =
    let
        validBoardIndexes =
            case game.activeBoard of
                Nothing ->
                    List.range 0 8

                Just idx ->
                    [ idx ]

        isValidBoardAndCell boardIdx cellIdx =
            isValidMove game boardIdx cellIdx
    in
    List.concatMap
        (\boardIdx ->
            List.map (\cellIdx -> ( boardIdx, cellIdx ))
                (List.range 0 8)
        )
        validBoardIndexes
        |> List.filter (\( boardIdx, cellIdx ) -> isValidBoardAndCell boardIdx cellIdx)
