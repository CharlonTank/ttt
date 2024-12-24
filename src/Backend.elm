module Backend exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontends)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Id exposing (Id(..), GameId(..))
import Lamdera
import List.Extra
import Random
import Types exposing (..)
import SeqDict as Dict exposing (SeqDict)
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
    Effect.Lamdera.onDisconnect PlayerDisconnected


init : ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { message = "Hello!"
      , matchmakingQueue = []
      , activeGames = Dict.empty
      , seed = Random.initialSeed 42
      , clientSessions = Dict.empty
      , clientToSession = Dict.empty
      }
    , Effect.Time.now |> Effect.Task.perform GotInitialTime
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        CheckForAbandon sessionId ->
            let
                sessionIdStr = Effect.Lamdera.sessionIdToString sessionId
                
                -- Only forfeit if the session still has no active clients
                shouldForfeit = 
                    not (Dict.member sessionIdStr model.clientSessions)
                        && isPlayerInGame sessionId model
            in
            if shouldForfeit then
                handleAbandonGame sessionId model
            else
                ( model, Command.none )

        _ ->
            ( model, Command.none )


isPlayerInGame : SessionId -> BackendModel -> Bool
isPlayerInGame sessionId model =
    SeqDict.toList model.activeGames
        |> List.any
            (\(_, game) ->
                game.player1 == sessionId || game.player2 == sessionId
            )


handleAbandonGame : SessionId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleAbandonGame sessionId model =
    let
        ( updatedGames, gameToUpdate ) =
            SeqDict.toList model.activeGames
                |> List.foldl
                    (\( gameId, game ) ( games, found ) ->
                        if game.player1 == sessionId then
                            ( SeqDict.insert gameId { game | board = setWinner O game.board } games
                            , Just ( gameId, game )
                            )
                        else if game.player2 == sessionId then
                            ( SeqDict.insert gameId { game | board = setWinner X game.board } games
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
                    Effect.Lamdera.broadcast (GameAbandoned gameId)
    in
    ( { model | activeGames = updatedGames }
    , command
    )


setWinner : Player -> BigBoard -> BigBoard
setWinner winner board =
    { board | winner = Just winner }


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        JoinMatchmaking ->
            handleJoinMatchmaking sessionId clientId model

        LeaveMatchmakingToBackend ->
            handleLeaveMatchmaking sessionId clientId model

        AbandonGame ->
            handleAbandonGame sessionId clientId model

        MakeMove boardIndex cellIndex player ->
            handleMakeMove sessionId clientId boardIndex cellIndex player model

        RequestBackendModel ->
            ( model, Effect.Lamdera.sendToFrontends sessionId (BackendModelReceived model) )

        ClientJoined cId sId ->
            let
                updatedClientSessions =
                    Dict.update (Effect.Lamdera.sessionIdToString sId)
                        (\mbClients ->
                            case mbClients of
                                Nothing ->
                                    Just [ Effect.Lamdera.clientIdToString cId ]

                                Just clients ->
                                    Just (Effect.Lamdera.clientIdToString cId :: clients)
                        )
                        model.clientSessions

                updatedClientToSession =
                    Dict.insert 
                        (Effect.Lamdera.clientIdToString cId) 
                        (Effect.Lamdera.sessionIdToString sId) 
                        model.clientToSession
            in
            ( { model 
              | clientSessions = updatedClientSessions
              , clientToSession = updatedClientToSession 
              }
            , Command.none
            )

        ClientDisconnected cId ->
            let
                clientIdStr = Effect.Lamdera.clientIdToString cId
                maybeSessionId = Dict.get clientIdStr model.clientToSession
                
                updatedClientToSession =
                    Dict.remove clientIdStr model.clientToSession

                updatedClientSessions =
                    case maybeSessionId of
                        Nothing ->
                            model.clientSessions

                        Just sessionIdStr ->
                            Dict.update sessionIdStr
                                (\mbClients ->
                                    case mbClients of
                                        Nothing ->
                                            Nothing

                                        Just clients ->
                                            let
                                                remainingClients =
                                                    List.filter ((/=) clientIdStr) clients
                                            in
                                            if List.isEmpty remainingClients then
                                                Nothing
                                            else
                                                Just remainingClients
                                )
                                model.clientSessions

                command =
                    case maybeSessionId of
                        Nothing ->
                            Command.none

                        Just sessionIdStr ->
                            if not (Dict.member sessionIdStr updatedClientSessions) then
                                -- All clients for this session are gone, schedule a check
                                Effect.Process.sleep (Duration.seconds 5)
                                    |> Effect.Task.perform 
                                        (\_ -> CheckForAbandon (Effect.Lamdera.sessionIdFromString sessionIdStr))
                            else
                                Command.none
            in
            ( { model 
              | clientSessions = updatedClientSessions
              , clientToSession = updatedClientToSession 
              }
            , command
            )


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

                    initialBoard =
                        { boards = List.repeat 9 { cells = List.repeat 9 Empty, winner = Nothing }
                        , currentPlayer = X
                        , activeBoard = Nothing
                        , winner = Nothing
                        , initialPlayer = X
                        , lastMove = Nothing
                        }

                    newGame : ActiveGame
                    newGame =
                        { id = Id gameUuid
                        , player1 = firstPlayer
                        , player2 = secondPlayer
                        , board = initialBoard
                        }

                    newModel =
                        { model
                            | matchmakingQueue = rest
                            , activeGames = Dict.insert newGame.id newGame model.activeGames
                            , seed = newSeed2
                        }
                in
                ( newModel
                , Command.batch
                    [ Effect.Lamdera.sendToFrontends secondPlayer (GameFound { opponentId = firstPlayer, playerRole = O })
                    , Effect.Lamdera.sendToFrontends firstPlayer (GameFound { opponentId = secondPlayer, playerRole = X })
                    ]
                )

            _ ->
                ( { model | matchmakingQueue = newQueue }
                , Command.none
                )


handleLeaveMatchmaking : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleLeaveMatchmaking sessionId _ model =
    ( { model | matchmakingQueue = List.filter ((/=) sessionId) model.matchmakingQueue }
    , Command.none
    )


handleAbandonGame : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleAbandonGame sessionId _ model =
    handleGameAbandon sessionId model


handleMakeMove : SessionId -> ClientId -> Int -> Int -> Player -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleMakeMove sessionId _ boardIndex cellIndex player model =
    let
        findGame =
            Dict.toList model.activeGames
                |> List.filter (\( _, game ) -> game.player1 == sessionId || game.player2 == sessionId)
                |> List.head
    in
    case findGame of
        Just ( _, game ) ->
            let
                opponent =
                    if game.player1 == sessionId then
                        game.player2
                    else
                        game.player1
            in
            ( model
            , Effect.Lamdera.sendToFrontends opponent (OpponentMove { boardIndex = boardIndex, cellIndex = cellIndex, player = player })
            )

        Nothing ->
            ( model, Command.none )
