module Tutorial.Tutorial exposing
    ( getTutorialBoard
    , isTutorialMoveValid
    )

import Audio exposing (Sound(..))
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import GameLogic exposing (isBigBoardComplete, makeMove)
import List.Extra as List
import Tutorial.Types exposing (TutorialStep(..))
import Types exposing (..)


getTutorialBoard : TutorialStep -> FrontendGame
getTutorialBoard step =
    case step of
        TutorialStep1 ->
            let
                boards =
                    List.indexedMap
                        (\i _ ->
                            if i == 4 then
                                emptySmallBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , self = Just X
            , currentPlayer = X
            , activeBoard = Just 4
            , winner = Nothing
            , lastMove = Nothing
            , botIsPlaying = False
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Nothing
            , opponent = BotOpponent Easy
            }

        TutorialStep2 ->
            let
                middleBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                boards =
                    List.indexedMap
                        (\i b ->
                            if i == 4 then
                                middleBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , currentPlayer = X
            , self = Just X
            , activeBoard = Just 2
            , winner = Nothing
            , lastMove = Nothing
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Nothing
            , botIsPlaying = False
            , opponent = BotOpponent Easy
            }

        TutorialStep3 ->
            let
                centerBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                topRightBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                bottomLeftBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                boards =
                    List.indexedMap
                        (\i b ->
                            if i == 4 then
                                centerBoard

                            else if i == 2 then
                                topRightBoard

                            else if i == 6 then
                                bottomLeftBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , self = Just X
            , currentPlayer = X
            , activeBoard = Just 4
            , winner = Nothing
            , lastMove = Nothing
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Nothing
            , botIsPlaying = False
            , opponent = BotOpponent Easy
            }

        TutorialStep4 ->
            let
                centerBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        ]
                    , winner = Just X
                    }

                topRightBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                bottomLeftBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                boards =
                    List.indexedMap
                        (\i b ->
                            if i == 4 then
                                centerBoard

                            else if i == 2 then
                                topRightBoard

                            else if i == 6 then
                                bottomLeftBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , self = Just O
            , currentPlayer = O
            , activeBoard = Nothing
            , winner = Nothing
            , lastMove = Nothing
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Nothing
            , botIsPlaying = False
            , opponent = BotOpponent Easy
            }

        TutorialStep5 ->
            let
                board0 =
                    { cells =
                        [ Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Filled X
                        , Filled O
                        , Empty
                        , Empty
                        , Filled X
                        ]
                    , winner = Just X -- Mark top-left board as won by X
                    }

                board1 =
                    { cells =
                        [ Filled O
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        ]
                    , winner = Nothing
                    }

                board2 =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        ]
                    , winner = Nothing
                    }

                board4 =
                    { cells =
                        [ Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        ]
                    , winner = Just X
                    }

                board5 =
                    { cells =
                        [ Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                board8 =
                    { cells =
                        [ Filled O
                        , Filled O
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        ]
                    , winner = Nothing
                    }

                boards =
                    List.indexedMap
                        (\i b ->
                            if i == 0 then
                                board0

                            else if i == 1 then
                                board1

                            else if i == 2 then
                                board2

                            else if i == 4 then
                                board4

                            else if i == 5 then
                                board5

                            else if i == 8 then
                                board8

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , self = Just X
            , currentPlayer = X
            , activeBoard = Just 8 -- Force play in bottom-right board
            , winner = Nothing
            , lastMove = Nothing
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Nothing
            , botIsPlaying = False
            , opponent = BotOpponent Easy
            }

        TutorialStep6 ->
            let
                board0 =
                    { cells =
                        [ Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Filled X
                        , Filled O
                        , Empty
                        , Empty
                        , Filled X
                        ]
                    , winner = Just X -- Mark top-left board as won by X
                    }

                board1 =
                    { cells =
                        [ Filled O
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        ]
                    , winner = Nothing
                    }

                board2 =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Filled O
                        ]
                    , winner = Nothing
                    }

                board4 =
                    { cells =
                        [ Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        ]
                    , winner = Just X
                    }

                board5 =
                    { cells =
                        [ Filled O
                        , Empty
                        , Empty
                        , Empty
                        , Filled X
                        , Empty
                        , Empty
                        , Empty
                        , Empty
                        ]
                    , winner = Nothing
                    }

                board8 =
                    { cells =
                        [ Filled O
                        , Filled O
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Filled X
                        , Empty
                        , Filled O
                        ]
                    , winner = Just X
                    }

                boards =
                    List.indexedMap
                        (\i b ->
                            if i == 0 then
                                board0

                            else if i == 1 then
                                board1

                            else if i == 2 then
                                board2

                            else if i == 4 then
                                board4

                            else if i == 5 then
                                board5

                            else if i == 8 then
                                board8

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { id = Nothing
            , boards = boards
            , self = Just X
            , currentPlayer = X
            , activeBoard = Nothing
            , winner = Just X
            , lastMove = Nothing
            , moveHistory = []
            , currentMoveIndex = 0
            , gameResult = Just Won
            , botIsPlaying = False
            , opponent = BotOpponent Easy
            }


isTutorialMoveValid : TutorialStep -> Int -> Int -> Bool
isTutorialMoveValid step boardIndex cellIndex =
    case step of
        TutorialStep1 ->
            boardIndex == 4 && cellIndex == 2

        TutorialStep2 ->
            boardIndex == 2

        TutorialStep3 ->
            boardIndex == 4 && cellIndex == 4

        TutorialStep4 ->
            True

        TutorialStep5 ->
            boardIndex == 8 && cellIndex == 6

        TutorialStep6 ->
            False


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }
