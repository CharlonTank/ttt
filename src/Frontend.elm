module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Lamdera
import List.Extra as List
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
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , board = initialBoard
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

        RestartGame ->
            ( { model | board = initialBoard }, Cmd.none )


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
    { title = "Ultimate Morpion"
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
            ]
            [ viewGame model ]
        ]
    }


viewGame : Model -> Html FrontendMsg
viewGame model =
    div 
        [ style "background-color" "white"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "width" "min(95vw, calc(100vh - 40px))"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "box-sizing" "border-box"
        , style "overflow" "hidden"
        ]
        [ div 
            [ style "padding" "20px 20px 0 20px"
            , style "flex" "0 0 auto"
            ]
            [ h1 
                [ style "margin" "0"
                , style "color" "#2c3e50"
                , style "font-size" "clamp(1.2em, 4vw, 2em)"
                , style "font-weight" "700"
                ] 
                [ text "Ultimate Morpion" ]
            , viewStatus model.board
            ]
        , div 
            [ style "flex" "1"
            , style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "min-height" "0"
            , style "padding" "10px 20px"
            , style "overflow" "hidden"
            ]
            [ viewBigBoard model.board ]
        , viewRestartButton
        ]


viewStatus : BigBoard -> Html msg
viewStatus board =
    h2 
        [ style "margin" "10px 0"
        , style "color" "#34495e"
        , style "font-size" "clamp(0.9em, 3vw, 1.2em)"
        , style "font-weight" "600"
        ]
        [ text <|
            case board.winner of
                Just player ->
                    "🎉 " ++ playerToString player ++ " wins! 🎉"

                Nothing ->
                    playerToString board.currentPlayer ++ "'s turn"
        ]


playerToString : Player -> String
playerToString player =
    case player of
        X ->
            "Player X"
            
        O ->
            "Player O"


viewBigBoard : BigBoard -> Html FrontendMsg
viewBigBoard board =
    div 
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "gap" "clamp(4px, 1vh, 10px)"
        , style "aspect-ratio" "1/1"
        , style "width" "100%"
        , style "height" "100%"
        , style "max-width" "100%"
        , style "max-height" "100%"
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
                "2px solid #4CAF50"
            else
                "2px solid #e2e8f0"

        backgroundColor =
            case smallBoard.winner of
                Just X ->
                    "#ff8a65"

                Just O ->
                    "#64b5f6"

                Nothing ->
                    "#ffffff"

        boxShadow =
            if isActive then
                "0 0 10px rgba(76, 175, 80, 0.3)"
            else
                "0 2px 4px rgba(0, 0, 0, 0.1)"
    in
    div
        [ style "border" borderColor
        , style "background-color" backgroundColor
        , style "border-radius" "8px"
        , style "transition" "all 0.3s ease"
        , style "box-shadow" boxShadow
        , style "display" "grid"
        , style "grid-template-columns" "repeat(3, 1fr)"
        , style "gap" "clamp(2px, 0.5vh, 4px)"
        , style "padding" "clamp(2px, 0.5vh, 4px)"
        , style "aspect-ratio" "1/1"
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
        [ style "border" "1px solid #e2e8f0"
        , style "border-radius" "4px"
        , style "background-color" "#ffffff"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "clamp(12px, 3vh, 24px)"
        , style "font-weight" "bold"
        , style "cursor" (if cellState == Empty then "pointer" else "default")
        , style "color" textColor
        , style "transition" "all 0.2s ease"
        , style "user-select" "none"
        , style "aspect-ratio" "1/1"
        , style "hover:background-color" hoverBg
        , onClick (CellClicked boardIndex cellIndex)
        ]
        [ text symbol ]


viewRestartButton : Html FrontendMsg
viewRestartButton =
    button
        [ style "width" "100%"
        , style "padding" "15px"
        , style "font-size" "clamp(12px, 2vh, 16px)"
        , style "font-weight" "600"
        , style "background-color" "#3498db"
        , style "color" "white"
        , style "border" "none"
        , style "border-top-left-radius" "0"
        , style "border-top-right-radius" "0"
        , style "border-bottom-left-radius" "20px"
        , style "border-bottom-right-radius" "20px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "margin-top" "auto"
        , onClick RestartGame
        ]
        [ text "Restart Game" ]