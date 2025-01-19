module Backend exposing (..)

import Crypto.Hash as Hash
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontends)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Elo exposing (getEloRating, updateEloRating)
import GameLogic exposing (checkBigBoardWinner, checkSmallBoardWinner, emptySmallBoard, isBigBoardComplete, isSmallBoardComplete)
import Id exposing (GameId(..), Id(..))
import Lamdera
import List.Extra
import Random
import SeqDict as Dict exposing (SeqDict)
import Types exposing (..)
import UUID


hashPassword : String -> String
hashPassword password =
    Hash.sha256 password


toPublicUser : User -> PublicUser
toPublicUser user =
    { id = user.id
    , name = user.name
    , elo = user.elo
    }


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
      , users = Dict.empty
      }
    , Command.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )

        ClientConnected sessionId clientId ->
            let
                updatedSessions : SeqDict SessionId Session
                updatedSessions =
                    model.sessions
                        |> Dict.update sessionId
                            (\mbSession ->
                                case mbSession of
                                    Just session ->
                                        Just { session | clientIds = clientId :: session.clientIds }

                                    Nothing ->
                                        Just { userId = Nothing, email = Nothing, clientIds = [ clientId ] }
                            )

                player =
                    getPlayer sessionId model
            in
            (if List.any (\p -> p == player) model.matchmakingQueue then
                ( model
                , Effect.Lamdera.sendToFrontend clientId JoinMatchmakingToFrontend
                )

             else
                let
                    fGame =
                        model.activeGames
                            |> Dict.filter (\_ g -> g.playerX == player || g.playerO == player)
                            |> Dict.values
                            |> List.head
                            |> Maybe.map (\g -> toFrontendOnlineGame player g)
                in
                case fGame of
                    Just game ->
                        ( model
                        , Effect.Lamdera.sendToFrontend clientId (SendGameToFrontend game)
                        )

                    Nothing ->
                        ( model
                        , Command.none
                        )
            )
                |> (\( newModel, cmds ) ->
                        ( { newModel | sessions = updatedSessions }
                        , Command.batch
                            [ cmds
                            , Effect.Lamdera.sendToFrontend clientId (SendUserToFrontend player)
                            ]
                        )
                   )

        ClientDisconnected sessionId clientId ->
            let
                player =
                    getPlayer sessionId model

                -- Update the session's clientIds, removing it if empty and anonymous
                updatedSessions =
                    model.sessions
                        |> Dict.update sessionId
                            (\maybeSession ->
                                maybeSession
                                    |> Maybe.andThen
                                        (\session ->
                                            let
                                                newClientIds =
                                                    List.filter ((/=) clientId) session.clientIds
                                            in
                                            case session.email of
                                                Just _ ->
                                                    -- Keep session for authenticated users even if empty
                                                    Just { session | clientIds = newClientIds }

                                                Nothing ->
                                                    -- Remove session for anonymous users if empty
                                                    if List.isEmpty newClientIds then
                                                        Nothing

                                                    else
                                                        Just { session | clientIds = newClientIds }
                                        )
                            )

                updatedQueue =
                    List.filter (\p -> p /= player) model.matchmakingQueue

                updatedModel =
                    { model
                        | sessions = updatedSessions
                        , matchmakingQueue = updatedQueue
                    }

                shouldDisconnectFromGame =
                    case player of
                        Authenticated _ ->
                            False

                        -- Never disconnect authenticated users
                        Anonymous _ _ ->
                            -- For anonymous: check if their only session is gone
                            Dict.get sessionId updatedSessions == Nothing
            in
            if shouldDisconnectFromGame && isPlayerInGame player model then
                disconnectFromGame sessionId player updatedModel

            else if model.matchmakingQueue /= updatedQueue then
                ( updatedModel
                , sendToClientsFromPlayer player LeftMatchmakingToFrontend updatedModel
                )

            else
                ( updatedModel, Command.none )


toFrontendOnlineGame : Player -> OnlineGameBackend -> FrontendOnlineGame
toFrontendOnlineGame self game =
    let
        selfSide =
            if game.playerX == self then
                X

            else
                O
    in
    { id = game.id
    , self = self
    , opponent =
        if selfSide == X then
            game.playerO

        else
            game.playerX
    , selfSide = selfSide
    , boards = game.boards
    , currentPlayer = game.currentPlayer
    , activeBoard = game.activeBoard
    , lastMove = game.lastMove
    , moveHistory = game.moveHistory
    , currentMoveIndex = game.currentMoveIndex
    , gameResult =
        case game.winner of
            Just X ->
                if selfSide == X then
                    Just Won

                else
                    Just Lost

            Just O ->
                if selfSide == O then
                    Just Won

                else
                    Just Lost

            Nothing ->
                if isBigBoardComplete game.boards then
                    Just Draw

                else
                    Nothing
    , winner = game.winner
    }


isPlayerInGame : Player -> BackendModel -> Bool
isPlayerInGame player model =
    Dict.toList model.activeGames
        |> List.any
            (\( _, game ) ->
                game.playerX == player || game.playerO == player
            )


handleGameAbandon : SessionId -> Id GameId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleGameAbandon sessionId gameId model =
    let
        player =
            getPlayer sessionId model
    in
    case Dict.get gameId model.activeGames of
        Nothing ->
            ( model, Command.none )

        Just game ->
            if game.playerX /= player && game.playerO /= player then
                ( model, Command.none )

            else
                let
                    -- Determine winner and loser
                    players =
                        if game.playerX == player then
                            { winner = game.playerO
                            , loser = game.playerX
                            }

                        else
                            { winner = game.playerX
                            , loser = game.playerO
                            }

                    -- Calculate ELO changes using Elo module
                    ( newWinnerElo, newLoserElo ) =
                        Elo.updateEloRatings
                            { winner = getEloRating players.winner
                            , loser = getEloRating players.loser
                            , isDraw = False
                            }

                    -- Update game with new ELO ratings
                    updatedGame =
                        if game.playerX == player then
                            { game
                                | winner = Just O
                                , playerX = updateEloRating game.playerX newLoserElo
                                , playerO = updateEloRating game.playerO newWinnerElo
                            }

                        else
                            { game
                                | winner = Just X
                                , playerX = updateEloRating game.playerX newWinnerElo
                                , playerO = updateEloRating game.playerO newLoserElo
                            }

                    command =
                        if game.playerX == player then
                            Command.batch
                                [ sendToClientsFromPlayer game.playerO (OpponentLeftToFrontend (toFrontendOnlineGame game.playerO updatedGame)) model
                                , sendToClientsFromPlayer game.playerX (OpponentLeftToFrontend (toFrontendOnlineGame game.playerX updatedGame)) model
                                ]

                        else
                            Command.batch
                                [ sendToClientsFromPlayer game.playerX (OpponentLeftToFrontend (toFrontendOnlineGame game.playerX updatedGame)) model
                                , sendToClientsFromPlayer game.playerO (OpponentLeftToFrontend (toFrontendOnlineGame game.playerO updatedGame)) model
                                ]
                in
                ( { model
                    | activeGames = Dict.remove gameId model.activeGames
                    , finishedGames = Dict.insert gameId updatedGame model.finishedGames
                  }
                , command
                )


disconnectFromGame : SessionId -> Player -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
disconnectFromGame sessionId player model =
    let
        maybeGameId =
            Dict.toList model.activeGames
                |> List.Extra.find (\( _, game ) -> game.playerX == player || game.playerO == player)
                |> Maybe.map Tuple.first
    in
    case maybeGameId of
        Nothing ->
            ( model, Command.none )

        Just gameId ->
            handleGameAbandon sessionId gameId model


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        JoinMatchmakingToBackend ->
            handleJoinMatchmaking sessionId clientId model

        LeaveMatchmakingToBackend ->
            handleLeaveMatchmaking sessionId clientId model

        AbandonGameToBackend gameId ->
            handleGameAbandon sessionId gameId model

        MakeMoveToBackend move ->
            handleMakeMove sessionId clientId move model

        RequestBackendModelToBackend ->
            ( model, Effect.Lamdera.sendToFrontends sessionId (BackendModelReceivedToFrontend model) )

        LoginOrSignUpToBackend email password ->
            case Dict.get email model.users of
                Just user ->
                    let
                        hashedPassword =
                            hashPassword password
                    in
                    if hashedPassword == user.encryptedPassword then
                        let
                            updatedSessions =
                                model.sessions
                                    |> Dict.updateIfExists sessionId
                                        (\maybeSession ->
                                            { maybeSession
                                                | email = Just email
                                                , userId = Just user.id
                                            }
                                        )

                            -- Remove anonymous player from matchmaking if they were in queue
                            anonymousPlayer =
                                Anonymous sessionId user.elo

                            updatedQueue =
                                List.filter (\p -> p /= anonymousPlayer) model.matchmakingQueue

                            updatedModel =
                                { model
                                    | sessions = updatedSessions
                                    , matchmakingQueue = updatedQueue
                                }
                        in
                        ( updatedModel
                        , Command.batch
                            [ Effect.Lamdera.sendToFrontend clientId (SignInDone (toPublicUser user))
                            , Effect.Lamdera.sendToFrontend clientId LeftMatchmakingToFrontend
                            ]
                        )

                    else
                        ( model
                        , Effect.Lamdera.sendToFrontend clientId (WrongPassword WrongPasswordError)
                        )

                Nothing ->
                    if String.length password < 6 then
                        ( model
                        , Effect.Lamdera.sendToFrontend clientId (WrongPassword PasswordTooShortError)
                        )

                    else
                        let
                            hashedPassword =
                                hashPassword password

                            ( newUserId, newSeed ) =
                                Random.step UUID.generator model.seed

                            newUser : User
                            newUser =
                                { id = Id newUserId
                                , email = email
                                , name = "Player " ++ String.left 5 email
                                , encryptedPassword = hashedPassword
                                , elo = 1000
                                }

                            updatedUsers =
                                Dict.insert email newUser model.users

                            updatedSessions =
                                model.sessions
                                    |> Dict.updateIfExists sessionId
                                        (\maybeSession ->
                                            { maybeSession
                                                | email = Just email
                                                , userId = Just (Id newUserId)
                                            }
                                        )
                        in
                        ( { model
                            | users = updatedUsers
                            , sessions = updatedSessions
                            , seed = newSeed
                          }
                        , Effect.Lamdera.sendToFrontend clientId (SignUpDone (toPublicUser newUser))
                        )

        LogOutToBackend ->
            let
                updatedSessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            maybeSession
                                |> Maybe.map
                                    (\session ->
                                        { session | email = Nothing, userId = Nothing }
                                    )
                        )
                        model.sessions

                clientIds =
                    Dict.get sessionId model.sessions
                        |> Maybe.map .clientIds
                        |> Maybe.withDefault []

                player =
                    getPlayer sessionId model

                userToAnonymous : Player
                userToAnonymous =
                    case player of
                        Authenticated publicUser ->
                            Anonymous sessionId publicUser.elo

                        Anonymous sid _ ->
                            Anonymous sid 1000

                -- Remove player from matchmaking queue
                updatedQueue =
                    List.filter (\p -> p /= player) model.matchmakingQueue

                updatedModel =
                    { model
                        | sessions = updatedSessions
                        , matchmakingQueue = updatedQueue
                    }
            in
            ( updatedModel
            , Command.batch
                (List.map
                    (\targetClientId -> Effect.Lamdera.sendToFrontend targetClientId (SendUserToFrontend userToAnonymous))
                    clientIds
                    ++ [ Effect.Lamdera.sendToFrontend clientId LeftMatchmakingToFrontend ]
                )
            )


handleJoinMatchmaking : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleJoinMatchmaking sessionId _ model =
    let
        player =
            getPlayer sessionId model
    in
    if
        List.any
            (\p ->
                case p of
                    Authenticated publicUser ->
                        case Dict.get sessionId model.sessions |> Maybe.andThen .userId of
                            Just userId ->
                                userId == publicUser.id

                            Nothing ->
                                False

                    Anonymous sid _ ->
                        sid == sessionId
            )
            model.matchmakingQueue
    then
        ( model, sendToClientsFromSessionId sessionId JoinMatchmakingToFrontend model )

    else
        let
            newQueue =
                player :: model.matchmakingQueue
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

                    getSessionId p =
                        case p of
                            Authenticated publicUser ->
                                Dict.toList model.sessions
                                    |> List.filter (\( _, session ) -> session.userId == Just publicUser.id)
                                    |> List.head
                                    |> Maybe.map Tuple.first
                                    |> Maybe.withDefault sessionId

                            Anonymous sid _ ->
                                sid

                    firstPlayerSessionId =
                        getSessionId firstPlayer

                    secondPlayerSessionId =
                        getSessionId secondPlayer

                    newGame : OnlineGameBackend
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
                    [ sendToClientsFromSessionId firstPlayerSessionId (SendGameToFrontend (toFrontendOnlineGame firstPlayer newGame)) newModel
                    , sendToClientsFromSessionId secondPlayerSessionId (SendGameToFrontend (toFrontendOnlineGame secondPlayer newGame)) newModel
                    ]
                )

            _ ->
                ( { model | matchmakingQueue = newQueue }
                , sendToClientsFromSessionId sessionId JoinMatchmakingToFrontend model
                )


initialOnlineGame : Id GameId -> Player -> Player -> OnlineGameBackend
initialOnlineGame id firstPlayer secondPlayer =
    { id = id
    , playerX = firstPlayer
    , playerO = secondPlayer
    , boards = List.repeat 9 emptySmallBoard
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
-- wonSmallBoardX : SmallBoard
-- wonSmallBoardX =
--     { cells = List.repeat 9 (Filled X)
--     , winner = Just X
--     }
-- wonSmallBoardO : SmallBoard
-- wonSmallBoardO =
--     { cells = List.repeat 9 (Filled O)
--     , winner = Just O
--     }
-- drawSmallBoards : List SmallBoard
-- drawSmallBoards =
--     [ wonSmallBoardX
--     , wonSmallBoardO
--     , wonSmallBoardO
--     , wonSmallBoardO
--     , wonSmallBoardX
--     , wonSmallBoardX
--     , wonSmallBoardX
--     , wonSmallBoardO
--     , emptySmallBoard
--     ]
-- emptySmallBoard : SmallBoard
-- emptySmallBoard =
--     { cells = List.repeat 9 Empty, winner = Nothing }


handleLeaveMatchmaking : SessionId -> ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleLeaveMatchmaking sessionId _ model =
    let
        player : Player
        player =
            getPlayer sessionId model
    in
    ( { model | matchmakingQueue = List.filter (\p -> p /= player) model.matchmakingQueue }
    , sendToClientsFromSessionId sessionId LeftMatchmakingToFrontend model
    )


handleMakeMove : SessionId -> ClientId -> Move -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleMakeMove sessionId _ move model =
    let
        player =
            getPlayer sessionId model
    in
    Dict.toList model.activeGames
        |> List.filter (\( _, game ) -> game.playerX == player || game.playerO == player)
        |> List.head
        |> Maybe.map
            (\( gameId, game ) ->
                let
                    opponent =
                        if game.playerX == player then
                            game.playerO

                        else
                            game.playerX

                    updatedBoards : List SmallBoard
                    updatedBoards =
                        List.indexedMap
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

                    updatedGame : OnlineGameBackend
                    updatedGame =
                        { game
                            | boards = updatedBoards
                            , currentPlayer =
                                if game.currentPlayer == X then
                                    O

                                else
                                    X
                            , lastMove = Just move
                            , moveHistory = game.moveHistory ++ [ move ]
                            , currentMoveIndex = List.length game.moveHistory
                            , winner = checkBigBoardWinner updatedBoards
                            , activeBoard =
                                if isSmallBoardComplete (List.Extra.getAt move.cellIndex updatedBoards |> Maybe.withDefault emptySmallBoard) then
                                    Nothing

                                else
                                    Just move.cellIndex
                        }

                    isGameFinished =
                        checkBigBoardWinner updatedBoards /= Nothing || isBigBoardComplete updatedBoards
                in
                if isGameFinished then
                    finishGame gameId updatedGame model

                else
                    ( { model | activeGames = Dict.insert gameId updatedGame model.activeGames }
                    , Command.batch
                        [ sendToClientsFromPlayer opponent (SendGameToFrontend (toFrontendOnlineGame opponent updatedGame)) model
                        , sendToClientsFromPlayer player (SendGameToFrontend (toFrontendOnlineGame player updatedGame)) model
                        ]
                    )
            )
        |> Maybe.withDefault ( model, Command.none )


finishGame : Id GameId -> OnlineGameBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
finishGame gameId game model =
    let
        isDraw =
            game.winner == Nothing && isBigBoardComplete game.boards

        -- Determine winner and loser
        players =
            case game.winner of
                Just X ->
                    { winner = game.playerX
                    , loser = game.playerO
                    }

                Just O ->
                    { winner = game.playerO
                    , loser = game.playerX
                    }

                Nothing ->
                    -- In case of draw, consider X as winner but use isDraw=true for Elo
                    { winner = game.playerX
                    , loser = game.playerO
                    }

        -- Calculate ELO changes using Elo module
        ( newWinnerElo, newLoserElo ) =
            Elo.updateEloRatings
                { winner = getEloRating players.winner
                , loser = getEloRating players.loser
                , isDraw = isDraw
                }

        -- Update game with new ELO ratings
        updatedGame =
            { game
                | playerX =
                    updateEloRating game.playerX
                        (if game.playerX == players.winner then
                            newWinnerElo

                         else
                            newLoserElo
                        )
                , playerO =
                    updateEloRating game.playerO
                        (if game.playerO == players.winner then
                            newWinnerElo

                         else
                            newLoserElo
                        )
            }

        updatedModel =
            { model
                | activeGames = Dict.remove gameId model.activeGames
                , finishedGames = Dict.insert gameId updatedGame model.finishedGames
            }
    in
    ( updatedModel
    , Command.batch
        [ sendToClientsFromPlayer game.playerO (SendFinishedGameToFrontend (toFrontendOnlineGame game.playerO updatedGame)) updatedModel
        , sendToClientsFromPlayer game.playerX (SendFinishedGameToFrontend (toFrontendOnlineGame game.playerX updatedGame)) updatedModel
        ]
    )



-- Helper function to get all client IDs for a user's session


getAllClientIdsFromSessionId : SessionId -> BackendModel -> List ClientId
getAllClientIdsFromSessionId sessionId model =
    Dict.get sessionId model.sessions
        |> Maybe.map .clientIds
        |> Maybe.withDefault []


getAllClientIdsFromPlayer : Player -> BackendModel -> List ClientId
getAllClientIdsFromPlayer player model =
    case player of
        Authenticated user ->
            Dict.toList model.sessions
                |> List.filter (\( _, session ) -> session.userId == Just user.id)
                |> List.concatMap (\( _, session ) -> session.clientIds)

        Anonymous sessionId _ ->
            getAllClientIdsFromSessionId sessionId model



-- Helper function to get all client IDs for a user (across all their sessions)


getAllUserClientIdsFromSessionId : SessionId -> BackendModel -> List ClientId
getAllUserClientIdsFromSessionId sessionId model =
    case Dict.get sessionId model.sessions |> Maybe.andThen .email of
        Just email ->
            -- User is authenticated, get all sessions with this email
            Dict.toList model.sessions
                |> List.filter (\( _, session ) -> session.email == Just email)
                |> List.concatMap (\( _, session ) -> session.clientIds)

        Nothing ->
            -- User is anonymous, just get their session's client IDs
            getAllClientIdsFromSessionId sessionId model



-- Helper function to send a message to all clients of a user


sendToClientsFromSessionId : SessionId -> ToFrontend -> BackendModel -> Command BackendOnly ToFrontend BackendMsg
sendToClientsFromSessionId sessionId msg model =
    Command.batch
        (List.map
            (\clientId -> Effect.Lamdera.sendToFrontend clientId msg)
            (getAllUserClientIdsFromSessionId sessionId model)
        )


sendToClientsFromPlayer : Player -> ToFrontend -> BackendModel -> Command BackendOnly ToFrontend BackendMsg
sendToClientsFromPlayer player msg model =
    Command.batch
        (List.map
            (\clientId -> Effect.Lamdera.sendToFrontend clientId msg)
            (getAllClientIdsFromPlayer player model)
        )


getPlayer : SessionId -> BackendModel -> Player
getPlayer sessionId model =
    case Dict.get sessionId model.sessions |> Maybe.andThen .email of
        Just email ->
            case Dict.get email model.users of
                Just user ->
                    Authenticated (toPublicUser user)

                Nothing ->
                    Anonymous sessionId 1000

        Nothing ->
            Anonymous sessionId 1000
