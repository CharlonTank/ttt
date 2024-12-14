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
            [ style "padding" "40px"
            , style "font-family" "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif"
            , style "min-height" "100vh"
            , style "background" "linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            ]
            [ viewGame model ]
        ]
    }


viewGame : Model -> Html FrontendMsg
viewGame model =
    div 
        [ style "text-align" "center"
        , style "background-color" "white"
        , style "border-radius" "20px"
        , style "box-shadow" "0 10px 30px rgba(0, 0, 0, 0.1)"
        , style "padding" "30px"
        , style "max-width" "800px"
        , style "width" "100%"
        ]
        [ h1 
            [ style "margin" "0 0 20px 0"
            , style "color" "#2c3e50"
            , style "font-size" "2.5em"
            , style "font-weight" "700"
            ] 
            [ text "Ultimate Morpion" ]
        , viewStatus model.board
        , viewBigBoard model.board
        , viewRestartButton
        ]


viewStatus : BigBoard -> Html msg
viewStatus board =
    h2 
        [ style "margin" "20px"
        , style "color" "#34495e"
        , style "font-size" "1.5em"
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
        [ style "display" "inline-block"
        , style "background-color" "#f8fafc"
        , style "border-radius" "15px"
        , style "padding" "20px"
        ]
        (List.groupsOf 3 (List.indexedMap (viewSmallBoard board) board.boards)
            |> List.map
                (\row ->
                    div [ style "display" "flex", style "gap" "15px", style "margin-bottom" "15px" ]
                        row
                )
        )


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
                "3px solid #4CAF50"
            else
                "3px solid #e2e8f0"

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
                "0 0 15px rgba(76, 175, 80, 0.3)"
            else
                "0 4px 6px rgba(0, 0, 0, 0.1)"
    in
    div
        [ style "border" borderColor
        , style "background-color" backgroundColor
        , style "padding" "12px"
        , style "border-radius" "12px"
        , style "transition" "all 0.3s ease"
        , style "box-shadow" boxShadow
        ]
        [ div []
            (List.groupsOf 3 (List.indexedMap (viewCell bigBoard boardIndex) smallBoard.cells)
                |> List.map
                    (\row ->
                        div [ style "display" "flex", style "gap" "8px", style "margin-bottom" "8px" ]
                            row
                    )
            )
        ]


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
        [ style "width" "55px"
        , style "height" "55px"
        , style "border" "2px solid #e2e8f0"
        , style "border-radius" "8px"
        , style "background-color" "#ffffff"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "36px"
        , style "font-weight" "bold"
        , style "cursor" (if cellState == Empty then "pointer" else "default")
        , style "color" textColor
        , style "transition" "all 0.2s ease"
        , style "user-select" "none"
        , style "hover:background-color" hoverBg
        , onClick (CellClicked boardIndex cellIndex)
        ]
        [ text symbol ]


viewRestartButton : Html FrontendMsg
viewRestartButton =
    button
        [ style "margin-top" "30px"
        , style "padding" "12px 24px"
        , style "font-size" "16px"
        , style "font-weight" "600"
        , style "background-color" "#3498db"
        , style "color" "white"
        , style "border" "none"
        , style "border-radius" "8px"
        , style "cursor" "pointer"
        , style "transition" "all 0.2s ease"
        , style "box-shadow" "0 4px 6px rgba(52, 152, 219, 0.2)"
        , style "hover:background-color" "#2980b9"
        , style "hover:transform" "translateY(-2px)"
        , style "hover:box-shadow" "0 6px 8px rgba(52, 152, 219, 0.3)"
        , onClick RestartGame
        ]
        [ text "Restart Game" ]