module Admin exposing (view)

import Color
import Effect.Lamdera exposing (SessionId)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n exposing (Translation)
import Id exposing (GameId, Id(..), display4CharsFromSessionId, display4CharsFromUUID)
import SeqDict as Dict exposing (SeqDict)
import Theme exposing (Theme)
import Types exposing (..)
import UUID


view : { t : Translation, c : Theme } -> Maybe BackendModel -> Html FrontendMsg
view { t, c } model =
    case model of
        Just backendModel ->
            div
                [ style "min-height" "100vh"
                , style "background-color" c.background
                , style "padding" "40px"
                , style "padding-left" "20px"
                , style "padding-right" "20px"
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
                            , style "@media (max-width: 600px)" "font-size: 20px"
                            ]
                            [ span 
                                [ style "font-size" "32px"
                                , style "@media (max-width: 600px)" "font-size: 24px"
                                ] 
                                [ text "🎮" ]
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
                                , style "@media (max-width: 600px)" "font-size: 18px"
                                ]
                                [ span 
                                    [ style "font-size" "24px"
                                    , style "@media (max-width: 600px)" "font-size: 18px"
                                    ] 
                                    [ text "🎲" ]
                                , text "Active Games"
                                ]
                            , viewActiveGames c backendModel.activeGames
                            ]
                        , div [ style "margin-bottom" "40px" ]
                            [ h2
                                [ style "margin" "0 0 20px 0"
                                , style "font-size" "24px"
                                , style "color" c.text
                                , style "display" "flex"
                                , style "align-items" "center"
                                , style "gap" "10px"
                                , style "@media (max-width: 600px)" "font-size: 18px"
                                ]
                                [ span 
                                    [ style "font-size" "24px"
                                    , style "@media (max-width: 600px)" "font-size: 18px"
                                    ] 
                                    [ text "👥" ]
                                , text "Matchmaking Queue"
                                ]
                            , viewMatchmakingQueue c backendModel.matchmakingQueue
                            ]
                        , div [ style "margin-bottom" "40px" ]
                            [ h2
                                [ style "margin" "0 0 20px 0"
                                , style "font-size" "24px"
                                , style "color" c.text
                                , style "display" "flex"
                                , style "align-items" "center"
                                , style "gap" "10px"
                                , style "@media (max-width: 600px)" "font-size: 18px"
                                ]
                                [ span 
                                    [ style "font-size" "24px"
                                    , style "@media (max-width: 600px)" "font-size: 18px"
                                    ] 
                                    [ text "🎲" ]
                                , text "Finished Games"
                                ]
                            , viewFinishedGames c backendModel.finishedGames
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text "SHOULD NOT HAPPEN"


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
            [ text (display4CharsFromSessionId sessionId) ]
        ]


viewActiveGames : Theme -> SeqDict (Id GameId) OnlineGame -> Html FrontendMsg
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
                        ]
                    ]
                , tbody []
                    (Dict.toList games |> List.map (viewActiveGameRow c))
                ]
            ]


viewActiveGameRow : Theme -> ( Id GameId, OnlineGame ) -> Html FrontendMsg
viewActiveGameRow c ( Id gameId, game ) =
    tr
        [ style "border-bottom" ("1px solid " ++ c.border) ]
        [ td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text <| display4CharsFromUUID gameId
                ]
            ]
        , td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text (display4CharsFromSessionId game.playerX) ]
            ]
        , td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text (display4CharsFromSessionId game.playerO) ]
            ]
        ]


viewFinishedGames : Theme -> SeqDict (Id GameId) OnlineGame -> Html FrontendMsg
viewFinishedGames c games =
    if Dict.isEmpty games then
        div
            [ style "padding" "20px"
            , style "background-color" c.background
            , style "border-radius" "10px"
            , style "text-align" "center"
            , style "color" c.text
            ]
            [ text "No finished games" ]

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
                    (Dict.toList games |> List.map (viewFinishedGameRow c))
                ]
            ]


viewFinishedGameRow : Theme -> ( Id GameId, OnlineGame ) -> Html FrontendMsg
viewFinishedGameRow c ( Id gameId, game ) =
    tr
        [ style "border-bottom" ("1px solid " ++ c.border) ]
        [ td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text <| display4CharsFromUUID gameId
                ]
            ]
        , td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text (display4CharsFromSessionId game.playerX) ]
            ]
        , td [ style "padding" "15px", style "color" c.text ]
            [ code
                [ style "background-color" (Color.withAlpha Color.primary 0.1)
                , style "padding" "4px 8px"
                , style "border-radius" "4px"
                , style "font-family" "monospace"
                , style "@media (max-width: 600px)" "font-size: 12px"
                ]
                [ text (display4CharsFromSessionId game.playerO) ]
            ]
        , td [ style "padding" "15px" ]
            [ div
                [ style "display" "inline-block"
                , style "padding" "6px 12px"
                , style "border-radius" "20px"
                , style "font-size" "14px"
                , style "@media (max-width: 600px)" "font-size: 12px"
                , style "background-color" (getStatusColor game.winner)
                , style "color" "white"
                ]
                [ text
                    (case game.winner of
                        Just winner ->
                            "Winner: " ++ playerToString winner

                        Nothing ->
                            "Draw"
                    )
                ]
            ]
        ]


getStatusColor : Maybe Player -> String
getStatusColor winner =
    case winner of
        Just X ->
            Color.danger

        Just O ->
            Color.primary

        Nothing ->
            Color.success


playerToString : Player -> String
playerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"
