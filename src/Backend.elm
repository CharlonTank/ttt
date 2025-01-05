module Backend exposing (..)

import Crypto.Hash as Hash
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontends)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Elo
import GameLogic exposing (checkBigBoardWinner, checkSmallBoardWinner, isBigBoardComplete, isSmallBoardComplete)
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
    { email = user.email
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
                                    Nothing ->
                                        Just { email = Nothing, clientIds = [ clientId ], elo = 1000 }

                                    Just session ->
                                        Just { session | clientIds = clientId :: session.clientIds }
                            )

                maybeCurrentUser : Maybe PublicUser
                maybeCurrentUser =
                    model.sessions
                        |> Dict.get sessionId
                        |> Maybe.andThen
                            (.email >> Maybe.andThen (\email -> Dict.get email model.users))
                        |> Maybe.map toPublicUser
            in
            (if List.member sessionId model.matchmakingQueue then
                ( model
                , Effect.Lamdera.sendToFrontend clientId AlreadyInMatchmakingToFrontend
                )

             else
                let
                    fGame =
                        model.activeGames
                            |> Dict.filter (\_ g -> g.playerX == sessionId || g.playerO == sessionId)
                            |> Dict.values
                            |> List.head
                            |> Maybe.map (\g -> toFrontendGame ( sessionId, Dict.get sessionId model.sessions |> Maybe.map .elo |> Maybe.withDefault 1000 ) g)
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
                            , Effect.Lamdera.sendToFrontend clientId (SendUserToFrontend maybeCurrentUser)
                            ]
                        )
                   )

        ClientDisconnected sessionId clientId ->
            let
                updatedSessions =
                    Dict.updateIfExists sessionId
                        (\session ->
                            { session | clientIds = List.filter ((/=) clientId) session.clientIds }
                        )
                        model.sessions

                isLastClient =
                    case Dict.get sessionId updatedSessions of
                        Nothing ->
                            True

                        Just session ->
                            List.isEmpty session.clientIds
            in
            if isLastClient then
                let
                    updatedModel =
                        { model
                            | sessions = updatedSessions
                            , matchmakingQueue = List.filter ((/=) sessionId) model.matchmakingQueue
                        }
                in
                if isPlayerInGame sessionId updatedModel then
                    disconnectFromGame sessionId updatedModel

                else
                    ( updatedModel, Command.none )

            else
                ( { model | sessions = updatedSessions }, Command.none )


toFrontendGame : ( SessionId, Int ) -> OnlineGame -> FrontendGame
toFrontendGame ( sessionId, elo ) game =
    let
        ( self, opponent ) =
            if game.playerX == sessionId then
                ( ( X, elo ), OnlineOpponent ( game.playerO, game.eloO ) )

            else
                ( ( O, elo ), OnlineOpponent ( game.playerX, game.eloX ) )
    in
    { id = Just game.id
    , self = Just self
    , opponent = opponent
    , boards = game.boards
    , currentPlayer = game.currentPlayer
    , activeBoard = game.activeBoard
    , lastMove = game.lastMove
    , moveHistory = game.moveHistory
    , currentMoveIndex = game.currentMoveIndex
    , gameResult =
        case game.winner of
            Just _ ->
                if self == ( X, elo ) then
                    Just Won

                else
                    Just Lost

            Nothing ->
                if isBigBoardComplete game.boards then
                    Just Draw

                else
                    Nothing
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


handleGameAbandon : Id GameId -> SessionId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleGameAbandon gameId sessionId model =
    case Dict.get gameId model.activeGames of
        Nothing ->
            ( model, Command.none )

        Just game ->
            if game.playerX /= sessionId && game.playerO /= sessionId then
                ( model, Command.none )

            else
                let
                    -- Determine winner and loser
                    players =
                        if game.playerX == sessionId then
                            { winnerSession = game.playerO
                            , winnerElo = game.eloO
                            , loserSession = game.playerX
                            , loserElo = game.eloX
                            }

                        else
                            { winnerSession = game.playerX
                            , winnerElo = game.eloX
                            , loserSession = game.playerO
                            , loserElo = game.eloO
                            }

                    -- Calculate ELO changes using Elo module
                    ( newWinnerElo, newLoserElo ) =
                        Elo.updateEloRatings
                            { winner = players.winnerElo
                            , loser = players.loserElo
                            , isDraw = False
                            }

                    -- Update sessions with new ELO ratings
                    updatedSessions =
                        model.sessions
                            |> Dict.update players.winnerSession
                                (\mbSession ->
                                    case mbSession of
                                        Just session ->
                                            Just { session | elo = newWinnerElo }

                                        Nothing ->
                                            Nothing
                                )
                            |> Dict.update players.loserSession
                                (\mbSession ->
                                    case mbSession of
                                        Just session ->
                                            Just { session | elo = newLoserElo }

                                        Nothing ->
                                            Nothing
                                )

                    updatedGame =
                        if game.playerX == sessionId then
                            { game
                                | winner = Just O
                                , eloO = newWinnerElo
                                , eloX = newLoserElo
                            }

                        else
                            { game
                                | winner = Just X
                                , eloX = newWinnerElo
                                , eloO = newLoserElo
                            }

                    command =
                        if game.playerX == sessionId then
                            Command.batch
                                [ Effect.Lamdera.sendToFrontends game.playerO (OpponentLeftToFrontend (toFrontendGame ( game.playerO, newWinnerElo ) updatedGame))
                                , Effect.Lamdera.sendToFrontends game.playerX (OpponentLeftToFrontend (toFrontendGame ( game.playerX, newLoserElo ) updatedGame))
                                ]

                        else
                            Command.batch
                                [ Effect.Lamdera.sendToFrontends game.playerX (OpponentLeftToFrontend (toFrontendGame ( game.playerX, newWinnerElo ) updatedGame))
                                , Effect.Lamdera.sendToFrontends game.playerO (OpponentLeftToFrontend (toFrontendGame ( game.playerO, newLoserElo ) updatedGame))
                                ]
                in
                ( { model
                    | activeGames = Dict.remove gameId model.activeGames
                    , finishedGames = Dict.insert gameId updatedGame model.finishedGames
                    , sessions = updatedSessions
                  }
                , command
                )


disconnectFromGame : SessionId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
disconnectFromGame sessionId model =
    let
        maybeGameId =
            Dict.toList model.activeGames
                |> List.Extra.find (\( _, game ) -> game.playerX == sessionId || game.playerO == sessionId)
                |> Maybe.map Tuple.first
    in
    case maybeGameId of
        Nothing ->
            ( model, Command.none )

        Just gameId ->
            handleGameAbandon gameId sessionId model


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
            handleGameAbandon gameId sessionId model

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
                                    |> Dict.update sessionId
                                        (\maybeSession ->
                                            Just
                                                { email = Just email
                                                , clientIds =
                                                    case maybeSession of
                                                        Just session ->
                                                            if List.member clientId session.clientIds then
                                                                session.clientIds

                                                            else
                                                                clientId :: session.clientIds

                                                        Nothing ->
                                                            [ clientId ]
                                                , elo = user.elo
                                                }
                                        )
                        in
                        ( { model | sessions = updatedSessions }
                        , Effect.Lamdera.sendToFrontend clientId (SignInDone (toPublicUser user))
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

                            newUser =
                                { email = email
                                , name = "Player " ++ String.left 5 email
                                , encryptedPassword = hashedPassword
                                , elo = 1000
                                }

                            updatedUsers =
                                Dict.insert email newUser model.users

                            updatedSessions =
                                Dict.update sessionId
                                    (\maybeSession ->
                                        Just
                                            { email = Just email
                                            , clientIds =
                                                case maybeSession of
                                                    Just session ->
                                                        if List.member clientId session.clientIds then
                                                            session.clientIds

                                                        else
                                                            clientId :: session.clientIds

                                                    Nothing ->
                                                        [ clientId ]
                                            , elo = 1000
                                            }
                                    )
                                    model.sessions
                        in
                        ( { model
                            | users = updatedUsers
                            , sessions = updatedSessions
                          }
                        , Effect.Lamdera.sendToFrontend clientId (SignUpDone (toPublicUser newUser))
                        )

        LogOutToBackend ->
            let
                updatedSessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Just session ->
                                    Just { session | email = Nothing }

                                Nothing ->
                                    Nothing
                        )
                        model.sessions

                clientIds =
                    Dict.get sessionId model.sessions
                        |> Maybe.map .clientIds
                        |> Maybe.withDefault []
            in
            ( { model | sessions = updatedSessions }
            , Command.batch
                (List.map
                    (\targetClientId -> Effect.Lamdera.sendToFrontend targetClientId (SendUserToFrontend Nothing))
                    clientIds
                )
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

                    newGame : OnlineGame
                    newGame =
                        initialOnlineGame (Id gameUuid) firstPlayer secondPlayer model

                    newModel =
                        { model
                            | matchmakingQueue = rest
                            , activeGames = Dict.insert newGame.id newGame model.activeGames
                            , seed = newSeed2
                        }
                in
                ( newModel
                , Command.batch
                    [ Effect.Lamdera.sendToFrontends secondPlayer (SendGameToFrontend (toFrontendGame ( secondPlayer, newGame.eloO ) newGame))
                    , Effect.Lamdera.sendToFrontends firstPlayer (SendGameToFrontend (toFrontendGame ( firstPlayer, newGame.eloX ) newGame))
                    ]
                )

            _ ->
                ( { model | matchmakingQueue = newQueue }
                , Command.none
                )


initialOnlineGame : Id GameId -> SessionId -> SessionId -> BackendModel -> OnlineGame
initialOnlineGame id firstPlayer secondPlayer model =
    let
        firstPlayerElo =
            Dict.get firstPlayer model.sessions
                |> Maybe.map .elo
                |> Maybe.withDefault 1000

        secondPlayerElo =
            Dict.get secondPlayer model.sessions
                |> Maybe.map .elo
                |> Maybe.withDefault 1000
    in
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
    , eloX = firstPlayerElo
    , eloO = secondPlayerElo
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
                    [ Effect.Lamdera.sendToFrontends opponent (SendGameToFrontend (toFrontendGame ( opponent, Dict.get opponent model.sessions |> Maybe.map .elo |> Maybe.withDefault 1000 ) updatedGame))
                    , Effect.Lamdera.sendToFrontends sessionId (SendGameToFrontend (toFrontendGame ( sessionId, Dict.get sessionId model.sessions |> Maybe.map .elo |> Maybe.withDefault 1000 ) updatedGame))
                    ]
                )

        Nothing ->
            ( model, Command.none )


finishGame : Id GameId -> OnlineGame -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
finishGame gameId game model =
    let
        -- Get player emails and sessions for Elo updates
        playerXSession =
            Dict.get game.playerX model.sessions

        playerOSession =
            Dict.get game.playerO model.sessions

        playerXEmail =
            playerXSession |> Maybe.andThen .email

        playerOEmail =
            playerOSession |> Maybe.andThen .email

        isDraw =
            game.winner == Nothing && isBigBoardComplete game.boards

        ( newEloX, newEloO ) =
            Elo.updateEloRatings
                { winner = game.eloX
                , loser = game.eloO
                , isDraw = isDraw
                }

        updatedSessions =
            model.sessions
                |> (case playerXEmail of
                        Just _ ->
                            Dict.update game.playerX
                                (\mbSession ->
                                    mbSession
                                        |> Maybe.map (\s -> { s | elo = newEloX })
                                )

                        Nothing ->
                            identity
                   )
                |> (case playerOEmail of
                        Just _ ->
                            Dict.update game.playerO
                                (\mbSession ->
                                    mbSession
                                        |> Maybe.map (\s -> { s | elo = newEloO })
                                )

                        Nothing ->
                            identity
                   )

        updatedUsers =
            model.users
                |> (case playerXEmail of
                        Just emailX ->
                            Dict.update emailX
                                (\mbUser ->
                                    mbUser
                                        |> Maybe.map (\u -> { u | elo = newEloX })
                                )

                        Nothing ->
                            identity
                   )
                |> (case playerOEmail of
                        Just emailO ->
                            Dict.update emailO
                                (\mbUser ->
                                    mbUser
                                        |> Maybe.map (\u -> { u | elo = newEloO })
                                )

                        Nothing ->
                            identity
                   )

        updatedGame =
            { game | eloX = newEloX, eloO = newEloO }

        updatedModel =
            { model
                | activeGames = Dict.remove gameId model.activeGames
                , finishedGames = Dict.insert gameId updatedGame model.finishedGames
                , users = updatedUsers
                , sessions = updatedSessions
            }
    in
    ( updatedModel
    , Command.batch
        [ Effect.Lamdera.sendToFrontends game.playerO (SendFinishedGameToFrontend (toFrontendGame ( game.playerO, newEloO ) updatedGame))
        , Effect.Lamdera.sendToFrontends game.playerX (SendFinishedGameToFrontend (toFrontendGame ( game.playerX, newEloX ) updatedGame))
        ]
    )
