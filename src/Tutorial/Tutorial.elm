module Tutorial.Tutorial exposing
    ( getTutorialBoard
    , isTutorialMoveValid
    )

import List.Extra as List
import Tutorial.Types exposing (TutorialStep(..))
import Types exposing (..)


getTutorialBoard : TutorialStep -> BigBoard
getTutorialBoard step =
    case step of
        TutorialIntro ->
            { boards = List.repeat 9 emptySmallBoard
            , currentPlayer = X
            , activeBoard = Nothing
            , winner = Nothing
            , initialPlayer = X
            }

        TutorialBasicMove ->
            let
                centerBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
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
                                centerBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { boards = boards
            , currentPlayer = X
            , activeBoard = Just 4
            , winner = Nothing
            , initialPlayer = X
            }

        TutorialBoardSelection ->
            let
                topRightBoard =
                    { cells =
                        [ Empty
                        , Empty
                        , Empty
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
                            if i == 2 then
                                topRightBoard

                            else
                                emptySmallBoard
                        )
                        (List.repeat 9 emptySmallBoard)
            in
            { boards = boards
            , currentPlayer = X
            , activeBoard = Just 2
            , winner = Nothing
            , initialPlayer = X
            }

        TutorialWinningSmall ->
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
            { boards = boards
            , currentPlayer = X
            , activeBoard = Just 4
            , winner = Nothing
            , initialPlayer = X
            }

        TutorialWinningBig ->
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
            { boards = boards
            , currentPlayer = X
            , activeBoard = Just 8 -- Force play in bottom-right board
            , winner = Nothing
            , initialPlayer = X
            }

        TutorialFreeChoice ->
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
            { boards = boards
            , currentPlayer = O
            , activeBoard = Nothing
            , winner = Nothing
            , initialPlayer = X
            }


isTutorialMoveValid : TutorialStep -> Int -> Int -> BigBoard -> Bool
isTutorialMoveValid step boardIndex cellIndex board =
    case step of
        TutorialIntro ->
            True

        TutorialBasicMove ->
            boardIndex == 4 && cellIndex == 2

        TutorialBoardSelection ->
            boardIndex == 2

        TutorialWinningSmall ->
            boardIndex == 4 && cellIndex == 4

        TutorialFreeChoice ->
            True

        -- Allow moves in any valid board since it's O's turn to play anywhere
        TutorialWinningBig ->
            boardIndex == 8 && cellIndex == 6


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty
    , winner = Nothing
    }