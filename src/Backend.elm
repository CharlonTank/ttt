module Backend exposing (..)

import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Effect.Subscription as Subscription exposing (Subscription)
import Effect.Task
import Effect.Time
import Lamdera
import List.Extra
import Random
import Types exposing (..)


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
      , activeGames = []
      , seed = Random.initialSeed 0
      }
    , Effect.Task.perform GotInitialTime Effect.Time.now
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Command.none )

        GotInitialTime time ->
            ( { model | seed = Random.initialSeed (Effect.Time.posixToMillis time) }
            , Command.none
            )

        PlayerDisconnected _ clientId ->
            handleGameAbandon clientId { model | matchmakingQueue = List.filter ((/=) clientId) model.matchmakingQueue }


handleGameAbandon : ClientId -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
handleGameAbandon clientId model =
    let
        findGame =
            List.filter
                (\( player1, player2, _ ) ->
                    player1 == clientId || player2 == clientId
                )
                model.activeGames
                |> List.head
    in
    case findGame of
        Just ( player1, player2, _ ) ->
            let
                opponent =
                    if player1 == clientId then
                        player2

                    else
                        player1

                updatedGames =
                    List.filter
                        (\( p1, p2, _ ) ->
                            not (p1 == player1 && p2 == player2)
                        )
                        model.activeGames
            in
            ( { model | activeGames = updatedGames }
            , Effect.Lamdera.sendToFrontend opponent OpponentLeft
            )

        Nothing ->
            ( model, Command.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Command.none )

        JoinMatchmaking ->
            if List.member clientId model.matchmakingQueue then
                ( model, Command.none )

            else
                let
                    newQueue =
                        clientId :: model.matchmakingQueue
                in
                case newQueue of
                    player1 :: player2 :: rest ->
                        let
                            ( randomInt, newSeed ) =
                                Random.step (Random.int 0 1) model.seed

                            ( firstPlayer, secondPlayer ) =
                                if randomInt == 0 then
                                    ( player1, player2 )

                                else
                                    ( player2, player1 )

                            initialBoard =
                                { boards = List.repeat 9 { cells = List.repeat 9 Empty, winner = Nothing }
                                , currentPlayer = X
                                , activeBoard = Nothing
                                , winner = Nothing
                                , initialPlayer = X
                                }

                            newModel =
                                { model
                                    | matchmakingQueue = rest
                                    , activeGames = ( firstPlayer, secondPlayer, initialBoard ) :: model.activeGames
                                    , seed = newSeed
                                }
                        in
                        ( newModel
                        , Command.batch
                            [ Effect.Lamdera.sendToFrontend secondPlayer (GameFound { opponentId = firstPlayer, playerRole = O })
                            , Effect.Lamdera.sendToFrontend firstPlayer (GameFound { opponentId = secondPlayer, playerRole = X })
                            ]
                        )

                    _ ->
                        ( { model | matchmakingQueue = newQueue }
                        , Command.none
                        )

        LeaveMatchmakingToBackend ->
            ( { model | matchmakingQueue = List.filter ((/=) clientId) model.matchmakingQueue }
            , Command.none
            )

        AbandonGame ->
            handleGameAbandon clientId model

        MakeMove boardIndex cellIndex player ->
            let
                findGame =
                    List.filter
                        (\( player1, player2, _ ) ->
                            player1 == clientId || player2 == clientId
                        )
                        model.activeGames
                        |> List.head
            in
            case findGame of
                Just ( player1, player2, _ ) ->
                    let
                        opponent =
                            if player1 == clientId then
                                player2

                            else
                                player1
                    in
                    ( model
                    , Effect.Lamdera.sendToFrontend opponent (OpponentMove { boardIndex = boardIndex, cellIndex = cellIndex, player = player })
                    )

                Nothing ->
                    ( model, Command.none )
