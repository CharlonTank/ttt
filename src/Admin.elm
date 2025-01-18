module Admin exposing (view)

import Color
import Effect.Lamdera exposing (ClientId, SessionId)
import Html exposing (..)
import Html.Attributes exposing (..)
import I18n exposing (Translation)
import Id exposing (GameId, Id(..), display4CharsFromClientId, display4CharsFromSessionId, display4CharsFromUUID)
import SeqDict as Dict exposing (SeqDict)
import Theme exposing (Theme)
import Types exposing (..)
import UUID


type alias Section =
    { title : String
    , emoji : String
    , content : Html FrontendMsg
    }


view : { t : Translation, c : Theme } -> Maybe BackendModel -> Html FrontendMsg
view { t, c } model =
    case model of
        Just backendModel ->
            div []
                [ viewStyles
                , div
                    [ class "admin-container"
                    , style "min-height" "100vh"
                    , style "background-color" c.background
                    , style "padding" "20px"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    ]
                    [ div
                        [ style "background-color" c.secondaryBackground
                        , style "border-radius" "15px"
                        , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
                        , style "overflow" "hidden"
                        , style "width" "100%"
                        , style "max-width" "1600px"
                        ]
                        [ viewHeader
                        , div [ style "padding" "20px" ]
                            (List.map (viewSection c)
                                [ { title = "Active Games"
                                  , emoji = "🎲"
                                  , content = viewActiveGames c backendModel.activeGames
                                  }
                                , { title = "Matchmaking Queue"
                                  , emoji = "👥"
                                  , content = viewMatchmakingQueue c backendModel.matchmakingQueue
                                  }
                                , { title = "Sessions"
                                  , emoji = "🔑"
                                  , content = viewSessions c backendModel.sessions
                                  }
                                , { title = "Finished Games"
                                  , emoji = "🏆"
                                  , content = viewFinishedGames c backendModel.finishedGames
                                  }
                                , { title = "Users"
                                  , emoji = "👥"
                                  , content = viewUsers c backendModel.users
                                  }
                                ]
                            )
                        ]
                    ]
                ]

        Nothing ->
            div [] [ text "Loading..." ]


viewStyles : Html msg
viewStyles =
    node "style"
        []
        [ text """
            .admin-table-wrapper {
                overflow-x: auto;
                width: 100%;
                border-radius: 10px;
                box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
            }
            .admin-table {
                min-width: 500px;
                background: transparent;
            }
            .admin-table td, .admin-table th {
                white-space: nowrap;
                transition: background-color 0.2s ease;
            }
            .admin-table tr:hover td {
                background-color: rgba(0, 0, 0, 0.02);
            }
            .admin-table code {
                padding: 4px 8px;
            }
            @media (max-width: 600px) {
                .admin-title { font-size: 20px !important; }
                .admin-emoji { font-size: 24px !important; }
                .admin-section-title { font-size: 18px !important; }
                .admin-section-emoji { font-size: 18px !important; }
                .admin-table { font-size: 12px !important; }
            }
        """ ]


viewHeader : Html msg
viewHeader =
    div
        [ style "padding" "40px"
        , style "background-color" Color.primary
        , style "color" "white"
        , style "box-shadow" "0 2px 10px rgba(0, 0, 0, 0.1)"
        ]
        [ h1
            [ class "admin-title"
            , style "margin" "0"
            , style "font-size" "32px"
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "15px"
            , style "justify-content" "center"
            ]
            [ span
                [ class "admin-emoji"
                , style "font-size" "36px"
                ]
                [ text "🎮" ]
            , text "Admin Dashboard"
            ]
        ]


viewSection : Theme -> Section -> Html FrontendMsg
viewSection c section =
    div
        [ style "margin-bottom" "40px"
        , style "animation" "fadeIn 0.5s ease-out"
        ]
        [ h2
            [ class "admin-section-title"
            , style "margin" "0 0 25px 0"
            , style "font-size" "26px"
            , style "color" c.text
            , style "display" "flex"
            , style "align-items" "center"
            , style "gap" "12px"
            ]
            [ span
                [ class "admin-section-emoji"
                , style "font-size" "28px"
                ]
                [ text section.emoji ]
            , text section.title
            ]
        , section.content
        ]


type alias TableConfig msg =
    { headers : List String
    , rows : List (List (Html msg))
    , emptyMessage : String
    }


viewTable : Theme -> TableConfig msg -> Html msg
viewTable c config =
    if List.isEmpty config.rows then
        div
            [ style "padding" "30px"
            , style "background-color" c.background
            , style "border-radius" "12px"
            , style "text-align" "center"
            , style "color" c.text
            , style "font-size" "16px"
            , style "box-shadow" "0 2px 8px rgba(0, 0, 0, 0.05)"
            ]
            [ text config.emptyMessage ]

    else
        div
            [ style "background-color" c.background
            , style "border-radius" "12px"
            , style "overflow" "hidden"
            , style "box-shadow" "0 2px 8px rgba(0, 0, 0, 0.05)"
            ]
            [ div [ class "admin-table-wrapper" ]
                [ table
                    [ class "admin-table"
                    , style "width" "100%"
                    , style "border-collapse" "collapse"
                    ]
                    [ thead []
                        [ tr
                            [ style "background-color" (Color.withAlpha Color.primary 0.1)
                            , style "border-bottom" ("2px solid " ++ c.border)
                            ]
                            (List.map (viewTableHeader c) config.headers)
                        ]
                    , tbody []
                        (List.map (viewTableRow c) config.rows)
                    ]
                ]
            ]


viewTableHeader : Theme -> String -> Html msg
viewTableHeader c text_ =
    th
        [ style "padding" "18px 18px 18px 24px"
        , style "text-align" "left"
        , style "color" c.text
        , style "font-weight" "600"
        , style "font-size" "14px"
        , style "letter-spacing" "0.5px"
        ]
        [ text text_ ]


viewTableRow : Theme -> List (Html msg) -> Html msg
viewTableRow c cells =
    tr
        [ style "border-bottom" ("1px solid " ++ c.border)
        , style "transition" "background-color 0.2s ease"
        ]
        (List.map (viewTableCell c) cells)


viewTableCell : Theme -> Html msg -> Html msg
viewTableCell c content =
    td
        [ style "padding" "16px 18px"
        , style "color" c.text
        ]
        [ content ]


viewCode : Theme -> String -> Html msg
viewCode c content =
    code
        [ style "background-color" (Color.withAlpha Color.primary 0.1)
        , style "border-radius" "6px"
        , style "font-family" "monospace"
        , style "padding" "4px 8px"
        , style "font-size" "14px"
        , style "letter-spacing" "0.5px"
        ]
        [ text content ]


viewMatchmakingQueue : Theme -> List Player -> Html FrontendMsg
viewMatchmakingQueue c queue =
    viewTable c
        { headers = [ "Position", "Player", "Type" ]
        , rows =
            List.indexedMap
                (\index player ->
                    [ text (String.fromInt (index + 1))
                    , case player of
                        Authenticated publicUser ->
                            viewCode c publicUser.name

                        Anonymous sessionId _ ->
                            viewCode c (display4CharsFromSessionId sessionId)
                    , case player of
                        Authenticated _ ->
                            text "Authenticated"

                        Anonymous _ _ ->
                            text "Anonymous"
                    ]
                )
                queue
        , emptyMessage = "No players in queue"
        }


viewActiveGames : Theme -> SeqDict (Id GameId) OnlineGameBackend -> Html FrontendMsg
viewActiveGames c games =
    viewTable c
        { headers = [ "ID", "P1", "P2" ]
        , rows =
            Dict.toList games
                |> List.map
                    (\( Id gameId, game ) ->
                        [ viewCode c (display4CharsFromUUID gameId)
                        , case game.playerX of
                            Authenticated user -> viewCode c user.name
                            Anonymous sessionId _ -> viewCode c (display4CharsFromSessionId sessionId)
                        , case game.playerO of
                            Authenticated user -> viewCode c user.name
                            Anonymous sessionId _ -> viewCode c (display4CharsFromSessionId sessionId)
                        ]
                    )
        , emptyMessage = "No active games"
        }


viewFinishedGames : Theme -> SeqDict (Id GameId) OnlineGameBackend -> Html FrontendMsg
viewFinishedGames c games =
    viewTable c
        { headers = [ "ID", "P1", "P2", "State" ]
        , rows =
            Dict.toList games
                |> List.map
                    (\( Id gameId, game ) ->
                        [ viewCode c (display4CharsFromUUID gameId)
                        , case game.playerX of
                            Authenticated user -> viewCode c user.name
                            Anonymous sessionId _ -> viewCode c (display4CharsFromSessionId sessionId)
                        , case game.playerO of
                            Authenticated user -> viewCode c user.name
                            Anonymous sessionId _ -> viewCode c (display4CharsFromSessionId sessionId)
                        , viewGameStatus game
                        ]
                    )
        , emptyMessage = "No finished games"
        }


viewGameStatus : OnlineGameBackend -> Html msg
viewGameStatus game =
    div
        [ style "display" "inline-block"
        , style "padding" "6px 16px"
        , style "border-radius" "20px"
        , style "font-size" "14px"
        , style "background-color" Color.primary
        , style "color" "white"
        , style "font-weight" "500"
        , style "letter-spacing" "0.5px"
        , style "box-shadow" "0 2px 4px rgba(0, 0, 0, 0.1)"
        ]
        [ text
            (case game.winner of
                Just winner ->
                    "Winner: " ++ playerToString winner

                Nothing ->
                    "Draw"
            )
        ]


viewSessions : Theme -> SeqDict SessionId Session -> Html FrontendMsg
viewSessions c sessions =
    viewTable c
        { headers = [ "SessionID", "ClientIDs", "#Clients" ]
        , rows =
            Dict.toList sessions
                |> List.map
                    (\( sessionId, session ) ->
                        [ viewCode c (display4CharsFromSessionId sessionId)
                        , div []
                            [ case List.take 2 session.clientIds of
                                [] ->
                                    text "-"

                                [ clientId ] ->
                                    viewCode c (display4CharsFromClientId clientId)

                                clientId1 :: clientId2 :: _ ->
                                    div []
                                        [ viewCode c (display4CharsFromClientId clientId1)
                                        , text " "
                                        , viewCode c (display4CharsFromClientId clientId2)
                                        , if List.length session.clientIds > 2 then
                                            text " ..."

                                          else
                                            text ""
                                        ]
                            ]
                        , text (String.fromInt (List.length session.clientIds))
                        ]
                    )
        , emptyMessage = "No active sessions"
        }


playerToString : PlayerSide -> String
playerToString player =
    case player of
        X ->
            "X"

        O ->
            "O"


viewUsers : Theme -> SeqDict Email User -> Html FrontendMsg
viewUsers c users =
    viewTable c
        { headers = [ "Email", "Name" ]
        , rows =
            Dict.toList users
                |> List.map
                    (\( email, user ) ->
                        [ text email
                        , text user.name
                        ]
                    )
        , emptyMessage = "No users registered yet"
        }
