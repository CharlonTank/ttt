module Backend exposing (..)

import Crypto.Hash as Hash
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontends)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import GameLogic exposing (checkBigBoardWinner, checkSmallBoardWinner, isBigBoardComplete)
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
                                        Just { email = Nothing, clientIds = [ clientId ] }

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
                            |> Maybe.map (\g -> toFrontendGame sessionId g)
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


toFrontendGame : SessionId -> OnlineGame -> FrontendGame
toFrontendGame selfId game =
    let
        ( self, opponent ) =
            if game.playerX == selfId then
                ( X, OnlineOpponent game.playerO )

            else
                ( O, OnlineOpponent game.playerX )
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
                    updatedGame =
                        if game.playerX == sessionId then
                            { game | winner = Just O }

                        else
                            { game | winner = Just X }

                    command =
                        if game.playerX == sessionId then
                            Effect.Lamdera.sendToFrontends game.playerO (OpponentLeftToFrontend (toFrontendGame game.playerO updatedGame))

                        else
                            Effect.Lamdera.sendToFrontends game.playerX (OpponentLeftToFrontend (toFrontendGame game.playerX updatedGame))
                in
                ( { model
                    | activeGames = Dict.remove gameId model.activeGames
                    , finishedGames = Dict.insert gameId updatedGame model.finishedGames
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
                    if hashedPassword == user.password then
                        let
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
                                            }
                                    )
                                    model.sessions
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
                                , password = hashedPassword
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
                    }

                isGameFinished =
                    checkBigBoardWinner updatedBoards /= Nothing || isBigBoardComplete updatedBoards

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
            , Command.batch
                [ Effect.Lamdera.sendToFrontends opponent (SendGameToFrontend (toFrontendGame opponent updatedGame))
                , Effect.Lamdera.sendToFrontends sessionId (SendGameToFrontend (toFrontendGame sessionId updatedGame))
                ]
            )

        Nothing ->
            ( model, Command.none )
