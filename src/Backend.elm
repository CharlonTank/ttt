module Backend exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontends)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Id exposing (GameId(..), Id(..))
import Lamdera
import List.Extra
import GameLogic exposing (checkSmallBoardWinner, checkBigBoardWinner, isBigBoardComplete)
import Random
import SeqDict as Dict exposing (SeqDict)
import Types exposing (..)
import UUID


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = subscriptions
    }


subscriptions : BackendModel -> Subscription BackendOnly BackendMsg
subscriptions _ =
    Subscription.batch
        [ Effect.Lamdera.onDisconnect ClientDisconnected
        , Effect.Lamdera.onConnect ClientConnected
        ]


init : ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { matchmakingQueue = []
      , activeGames = Dict.empty
      , finishedGames = Dict.empty
      , seed = Random.initialSeed 89137219321
      , sessions = Dict.empty
      }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )

        ClientConnected sessionId clientId ->
            case Dict.get sessionId model.sessions of
                Just oldSession ->
                    case oldSession.activity of
                        InGame (Id existingGameId) ->
                            let
                                newSession =
                                    { oldSession
                                        | clientIds = clientId :: oldSession.clientIds
                                    }

                                updatedSessions =
                                    Dict.insert sessionId newSession model.sessions

                                maybeFrontendGame =
                                    model.activeGames
                                        |> Dict.get (Id existingGameId)
                                        |> Maybe.map (toFrontendGame sessionId)
                            in
                            ( { model | sessions = updatedSessions }
                            , case maybeFrontendGame of
                                Just frontendGame ->
                                    Effect.Lamdera.sendToFrontend clientId (SendGameToFrontend frontendGame)

                                Nothing ->
                                    Command.none
                            )

                        InQueue ->
                            -- If this session is already in the matchmaking queue:
                            let
                                newSession =
                                    { oldSession
                                        | clientIds = clientId :: oldSession.clientIds

                                        -- Possibly keep them in queue or do something else
                                    }

                                updatedSessions =
                                    Dict.insert sessionId newSession model.sessions
                            in
                            ( { model | sessions = updatedSessions }
                            , Command.none
                            )

                        Available ->
                            -- If this session was not doing anything special:
                            let
                                newSession =
                                    { oldSession
                                        | clientIds = clientId :: oldSession.clientIds

                                        -- Maybe switch them to “InQueue” or keep them “Available”
                                    }

                                updatedSessions =
                                    Dict.insert sessionId newSession model.sessions
                            in
                            ( { model | sessions = updatedSessions }
                            , Command.none
                            )

                Nothing ->
                    -- If we had no record for this session yet, create a brand new one:
                    let
                        newSession =
                            { clientIds = [ clientId ]
                            , activity = Available

                            -- or InQueue, or however you want them to start
                            }

                        updatedSessions =
                            Dict.insert sessionId newSession model.sessions
                    in
                    ( { model | sessions = updatedSessions }
                    , Command.none
                    )

        ClientDisconnected sessionId clientId ->
            case Dict.get sessionId model.sessions of
                Just session ->
                    let
                        shouldForfeit =
                            not (List.member clientId session.clientIds)
                                && isPlayerInGame sessionId model
                    in
                    if shouldForfeit then
                        handleGameAbandon sessionId model

                    else
                        ( model, Command.none )

                Nothing ->
                    ( model, Command.none )


toFrontendGame : SessionId -> OnlineGame -> FrontendGame
toFrontendGame selfId game =
    let
        ( self, opponent ) =
            if game.playerX == selfId then
                ( X, OnlineOpponent game.playerO )

            else
                ( O, OnlineOpponent game.playerX )
    in
    { self = Just self
    , opponent = opponent
    , boards = game.boards
    , currentPlayer = game.currentPlayer
    , activeBoard = game.activeBoard
    , lastMove = game.lastMove
    , moveHistory = game.moveHistory
    , currentMoveIndex = game.currentMoveIndex
    , gameResult = Nothing
    , botIsPlaying = False
    , winner = game.winner
    }


isPlayerInGame : SessionId -> BackendModel -> Bool
isPlayerInGame sessionId model =
    Dict.toList model.activeGames
        |> List.any
            (\( _, game ) ->
                game.playerX == sessionId || game.playerO == sessionId
            )


handleGameAbandon : SessionId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleGameAbandon sessionId model =
    let
        ( updatedGames, gameToUpdate ) =
            Dict.toList model.activeGames
                |> List.foldl
                    (\( gameId, game ) ( games, found ) ->
                        if game.playerX == sessionId then
                            ( Dict.update gameId (\mg -> mg |> Maybe.map (\g -> { g | winner = Just O })) games
                            , Just ( gameId, game )
                            )

                        else if game.playerO == sessionId then
                            ( Dict.update gameId (\mg -> mg |> Maybe.map (\g -> { g | winner = Just X })) games
                            , Just ( gameId, game )
                            )

                        else
                            ( games, found )
                    )
                    ( model.activeGames, Nothing )


        command =
            case gameToUpdate of
                Nothing ->
                    Command.none

                Just ( gameId, game ) ->
                    if game.playerX == sessionId then
                        Effect.Lamdera.sendToFrontends game.playerO (OpponentLeftToFrontend (toFrontendGame game.playerO game))
                    else
                        Effect.Lamdera.sendToFrontends game.playerX (OpponentLeftToFrontend (toFrontendGame game.playerX game))
    in
        ( { model | activeGames = updatedGames }
    , command
    )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        JoinMatchmakingToBackend ->
            handleJoinMatchmaking sessionId clientId model

        LeaveMatchmakingToBackend ->
            handleLeaveMatchmaking sessionId clientId model

        AbandonGameToBackend ->
            handleGameAbandon sessionId model

        MakeMoveToBackend move ->
            handleMakeMove sessionId clientId move model

        RequestBackendModelToBackend ->
            ( model, Effect.Lamdera.sendToFrontends sessionId (BackendModelReceivedToFrontend model) )



-- ClientJoinedToBackend cId sId ->
--     let
--         updatedClientSessions =
--             Dict.update (Effect.Lamdera.sessionIdToString sId)
--                 (\mbClients ->
--                     case mbClients of
--                         Nothing ->
--                             Just [ Effect.Lamdera.clientIdToString cId ]
--                         Just clients ->
--                             Just (Effect.Lamdera.clientIdToString cId :: clients)
--                 )
--                 model.clientSessions
--         updatedClientToSession =
--             Dict.insert
--                 (Effect.Lamdera.clientIdToString cId)
--                 (Effect.Lamdera.sessionIdToString sId)
--                 model.clientToSession
--     in
--     ( { model
--       | clientSessions = updatedClientSessions
--       , clientToSession = updatedClientToSession
--       }
--     , Command.none
--     )
-- ClientDisconnected cId ->
--     let
--         clientIdStr = Effect.Lamdera.clientIdToString cId
--         maybeSessionId = Dict.get clientIdStr model.clientToSession
--         updatedClientToSession =
--             Dict.remove clientIdStr model.clientToSession
--         updatedClientSessions =
--             case maybeSessionId of
--                 Nothing ->
--                     model.clientSessions
--                 Just sessionIdStr ->
--                     Dict.update sessionIdStr
--                         (\mbClients ->
--                             case mbClients of
--                                 Nothing ->
--                                     Nothing
--                                 Just clients ->
--                                     let
--                                         remainingClients =
--                                             List.filter ((/=) clientIdStr) clients
--                                     in
--                                     if List.isEmpty remainingClients then
--                                         Nothing
--                                     else
--                                         Just remainingClients
--                         )
--                         model.clientSessions
--         command =
--             case maybeSessionId of
--                 Nothing ->
--                     Command.none
--                 Just sessionIdStr ->
--                     if not (Dict.member sessionIdStr updatedClientSessions) then
--                         -- All clients for this session are gone, schedule a check
--                         Effect.Process.sleep (Duration.seconds 5)
--                             |> Effect.Task.perform
--                                 (\_ -> CheckForAbandon (Effect.Lamdera.sessionIdFromString sessionIdStr))
--                     else
--                         Command.none
--     in
--     ( { model
--       | clientSessions = updatedClientSessions
--       , clientToSession = updatedClientToSession
--       }
--     , command
--     )


handleJoinMatchmaking : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleJoinMatchmaking sessionId _ model =
    if List.member sessionId model.matchmakingQueue then
        ( model, Command.none )

    else
        let
            newQueue =
                sessionId :: model.matchmakingQueue
        in
        case newQueue of
            player1 :: player2 :: rest ->
                let
                    ( randomInt, newSeed1 ) =
                        Random.step (Random.int 0 1) model.seed

                    ( firstPlayer, secondPlayer ) =
                        if randomInt == 0 then
                            ( player1, player2 )

                        else
                            ( player2, player1 )

                    ( gameUuid, newSeed2 ) =
                        Random.step UUID.generator newSeed1

                    newGame : OnlineGame
                    newGame =
                        initialOnlineGame (Id gameUuid) firstPlayer secondPlayer

                    newModel =
                        { model
                            | matchmakingQueue = rest
                            , activeGames = Dict.insert newGame.id newGame model.activeGames
                            , seed = newSeed2
                        }
                in
                ( newModel
                , Command.batch
                    [ Effect.Lamdera.sendToFrontends secondPlayer (SendGameToFrontend (toFrontendGame secondPlayer newGame))
                    , Effect.Lamdera.sendToFrontends firstPlayer (SendGameToFrontend (toFrontendGame firstPlayer newGame))
                    ]
                )

            _ ->
                ( { model | matchmakingQueue = newQueue }
                , Command.none
                )


initialOnlineGame : Id GameId -> SessionId -> SessionId -> OnlineGame
initialOnlineGame id firstPlayer secondPlayer =
    { id = id
    , playerX = firstPlayer
    , playerO = secondPlayer
    , boards = List.repeat 9 { cells = List.repeat 9 Empty, winner = Nothing }
    , currentPlayer = X
    , activeBoard = Nothing
    , winner = Nothing
    , lastMove = Nothing
    , moveHistory = []
    , currentMoveIndex = -1
    }


-- initialFrontendGameOneStepFromDraw : Id GameId -> SessionId -> SessionId -> OnlineGame
-- initialFrontendGameOneStepFromDraw id firstPlayer secondPlayer =
--     { id = id
--     , playerX = firstPlayer
--     , playerO = secondPlayer
--     , boards = drawSmallBoards
--     , currentPlayer = X
--     , activeBoard = Nothing
--     , winner = Nothing
--     , lastMove = Just { boardIndex = 0, cellIndex = 0 }
--     , moveHistory = []
--     , currentMoveIndex = -1
--     }


wonSmallBoardX : SmallBoard
wonSmallBoardX =
    { cells = List.repeat 9 (Filled X)
    , winner = Just X
    }


wonSmallBoardO : SmallBoard
wonSmallBoardO =
    { cells = List.repeat 9 (Filled O)
    , winner = Just O
    }


drawSmallBoards : List SmallBoard
drawSmallBoards =
    [ wonSmallBoardX
    , wonSmallBoardO
    , wonSmallBoardO
    , wonSmallBoardO
    , wonSmallBoardX
    , wonSmallBoardX
    , wonSmallBoardX
    , wonSmallBoardO
    , emptySmallBoard
    ]


emptySmallBoard : SmallBoard
emptySmallBoard =
    { cells = List.repeat 9 Empty, winner = Nothing }


handleLeaveMatchmaking : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleLeaveMatchmaking sessionId _ model =
    ( { model | matchmakingQueue = List.filter ((/=) sessionId) model.matchmakingQueue }
    , Command.none
    )



handleMakeMove : SessionId -> ClientId -> Move -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleMakeMove sessionId _ move model =
    let
        findGame =
            Dict.toList model.activeGames
                |> List.filter (\( _, game ) -> game.playerX == sessionId || game.playerO == sessionId)
                |> List.head
    in
    case findGame of
        Just ( gameId, game ) ->
            let
                opponent =
                    if game.playerX == sessionId then
                        game.playerO

                    else
                        game.playerX

                updatedGame =
                    { game
                        | boards = List.indexedMap
                            (\i board ->
                                if i == move.boardIndex then
                                    let
                                        updatedCells =
                                            List.indexedMap
                                                (\j cell ->
                                                    if j == move.cellIndex then
                                                        Filled game.currentPlayer
                                                    else
                                                        cell
                                                )
                                                board.cells
                                    in
                                    { board
                                        | cells = updatedCells
                                        , winner = checkSmallBoardWinner updatedCells
                                    }
                                else
                                    board
                            )
                            game.boards
                        , currentPlayer = if game.currentPlayer == X then O else X
                        , lastMove = Just move
                        , moveHistory = game.moveHistory ++ [ move ]
                        , currentMoveIndex = List.length game.moveHistory
                    }

                isGameFinished =
                    checkBigBoardWinner updatedGame.boards /= Nothing || isBigBoardComplete updatedGame.boards

                updatedModel =
                    if isGameFinished then
                        { model
                            | activeGames = Dict.remove gameId model.activeGames
                            , finishedGames = Dict.insert gameId updatedGame model.finishedGames
                        }
                    else
                        { model
                            | activeGames = Dict.insert gameId updatedGame model.activeGames
                        }
            in
            ( updatedModel
            , Effect.Lamdera.sendToFrontends opponent (OpponentMoveToFrontend move)
            )

        Nothing ->
            ( model, Command.none )
