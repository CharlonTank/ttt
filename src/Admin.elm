module Admin exposing (view)

import Effect.Lamdera exposing (SessionId)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n exposing (Translation)
import Types exposing (..)
import Theme exposing (Theme)
import SeqDict as Dict exposing (SeqDict)
import Id exposing (Id, GameId)
import UUID
import Color


view : { t : Translation, c : Theme } -> BackendModel -> Html FrontendMsg
view { t, c } model =
    div 
        [ style "min-height" "100vh"
        , style "background-color" c.background
        , style "padding" "40px"
        ]
        [ div 
            [ style "max-width" "1200px"
            , style "margin" "0 auto"
            , style "background-color" c.secondaryBackground
            , style "border-radius" "15px"
            , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
            , style "overflow" "hidden"
            ]
            [ div 
                [ style "padding" "30px"
                , style "background-color" Color.primary
                , style "color" "white"
                ]
                [ h1 
                    [ style "margin" "0"
                    , style "font-size" "28px"
                    , style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "10px"
                    ] 
                    [ span [ style "font-size" "32px" ] [ text "🎮" ]
                    , text "Admin Dashboard"
                    ]
                ]
            , div [ style "padding" "30px" ]
                [ div [ style "margin-bottom" "40px" ]
                    [ h2 
                        [ style "margin" "0 0 20px 0"
                        , style "font-size" "24px"
                        , style "color" c.text
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "10px"
                        ] 
                        [ span [ style "font-size" "24px" ] [ text "🎲" ]
                        , text "Active Games"
                        ]
                    , viewActiveGames c model.activeGames
                    ]
                , div [ style "margin-bottom" "40px" ]
                    [ h2 
                        [ style "margin" "0 0 20px 0"
                        , style "font-size" "24px"
                        , style "color" c.text
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "10px"
                        ] 
                        [ span [ style "font-size" "24px" ] [ text "👥" ]
                        , text "Matchmaking Queue" 
                        ]
                    , viewMatchmakingQueue c model.matchmakingQueue
                    ]
                ]
            ]
        ]

viewMatchmakingQueue : Theme -> List SessionId -> Html FrontendMsg
viewMatchmakingQueue c queue =
    if List.isEmpty queue then
        div 
            [ style "padding" "20px"
            , style "background-color" c.background
            , style "border-radius" "10px"
            , style "text-align" "center"
            , style "color" c.text
            ] 
            [ text "No players in queue" ]
    else
        div 
            [ style "background-color" c.background
            , style "border-radius" "10px"
            , style "overflow" "hidden"
            ]
            [ table 
                [ style "width" "100%"
                , style "border-collapse" "collapse"
                ] 
                [ thead []
                    [ tr 
                        [ style "background-color" (Color.withAlpha Color.primary 0.1) ]
                        [ th 
                            [ style "padding" "15px"
                            , style "text-align" "left"
                            , style "color" c.text
                            ] 
                            [ text "Player ID" ]
                        ]
                    ]
                , tbody []
                    (List.map (viewQueueRow c) queue)
                ]
            ]

viewQueueRow : Theme -> SessionId -> Html FrontendMsg
viewQueueRow c sessionId =
    tr 
        [ style "border-bottom" ("1px solid " ++ c.border) ]
        [ td 
            [ style "padding" "15px"
            , style "color" c.text
            ] 
            [ text (Effect.Lamdera.sessionIdToString sessionId) ]
        ]

viewActiveGames : Theme -> SeqDict (Id GameId) ActiveGame -> Html FrontendMsg
viewActiveGames c games =
    if Dict.isEmpty games then
        div 
            [ style "padding" "20px"
            , style "background-color" c.background
            , style "border-radius" "10px"
            , style "text-align" "center"
            , style "color" c.text
            ] 
            [ text "No active games" ]
    else
        div 
            [ style "background-color" c.background
            , style "border-radius" "10px"
            , style "overflow" "hidden"
            ]
            [ table 
                [ style "width" "100%"
                , style "border-collapse" "collapse"
                ] 
                [ thead []
                    [ tr 
                        [ style "background-color" (Color.withAlpha Color.primary 0.1) ]
                        [ th [ style "padding" "15px", style "text-align" "left", style "color" c.text ] [ text "Game ID" ]
                        , th [ style "padding" "15px", style "text-align" "left", style "color" c.text ] [ text "Player 1" ]
                        , th [ style "padding" "15px", style "text-align" "left", style "color" c.text ] [ text "Player 2" ]
                        , th [ style "padding" "15px", style "text-align" "left", style "color" c.text ] [ text "Game State" ]
                        ]
                    ]
                , tbody []
                    (Dict.toList games |> List.map (viewGameRow c))
                ]
            ]

viewGameRow : Theme -> ( Id GameId, ActiveGame ) -> Html FrontendMsg
viewGameRow c ( gameId, game ) =
    tr 
        [ style "border-bottom" ("1px solid " ++ c.border) ]
        [ td [ style "padding" "15px", style "color" c.text ] 
            [ code 
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                ] 
                [ text (UUID.toString (case gameId of Id.Id uuid -> uuid)) ]
            ]
        , td [ style "padding" "15px", style "color" c.text ] 
            [ code 
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                ] 
                [ text (Effect.Lamdera.sessionIdToString game.player1) ]
            ]
        , td [ style "padding" "15px", style "color" c.text ] 
            [ code 
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                ] 
                [ text (Effect.Lamdera.sessionIdToString game.player2) ]
            ]
        , td [ style "padding" "15px" ] 
            [ div 
                [ style "display" "inline-block"
                , style "padding" "6px 12px"
                , style "border-radius" "20px"
                , style "font-size" "14px"
                , style "background-color" (getStatusColor game.board.winner)
                , style "color" "white"
                ] 
                [ text 
                    (case game.board.winner of
                        Just winner -> "Winner: " ++ playerToString winner
                        Nothing -> "In Progress - " ++ playerToString game.board.currentPlayer ++ "'s turn"
                    )
                ]
            ]
        ]

getStatusColor : Maybe Player -> String
getStatusColor winner =
    case winner of
        Just X -> Color.danger
        Just O -> Color.primary
        Nothing -> Color.success

playerToString : Player -> String
playerToString player =
    case player of
        X -> "X"
        O -> "O" 