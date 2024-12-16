module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Types exposing (..)
import List.Extra
import Time
import Random
import Task


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    BackendModel


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { message = "Hello!"
      , matchmakingQueue = []
      , activeGames = []
      , seed = Random.initialSeed 0
      }
    , Task.perform GotInitialTime Time.now
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )
            
        GotInitialTime time ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis time) }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )
            
        JoinMatchmaking ->
            if List.member clientId model.matchmakingQueue then
                ( model, Cmd.none )
            else
                let
                    newQueue = clientId :: model.matchmakingQueue
                in
                case newQueue of
                    player1 :: player2 :: rest ->
                        let
                            (randomInt, newSeed) = Random.step (Random.int 0 1) model.seed
                            (firstPlayer, secondPlayer) = 
                                if randomInt == 0 then
                                    (player1, player2)
                                else
                                    (player2, player1)
                                    
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
                        , Cmd.batch
                            [ sendToFrontend secondPlayer (GameFound { opponentId = firstPlayer, playerRole = O })
                            , sendToFrontend firstPlayer (GameFound { opponentId = secondPlayer, playerRole = X })
                            ]
                        )
                    
                    _ ->
                        ( { model | matchmakingQueue = newQueue }
                        , Cmd.none
                        )
                        
        LeaveMatchmaking ->
            ( { model | matchmakingQueue = List.filter ((/=) clientId) model.matchmakingQueue }
            , Cmd.none
            )
            
        AbandonGame ->
            let
                findGame =
                    List.filter
                        (\(player1, player2, _) ->
                            player1 == clientId || player2 == clientId
                        )
                        model.activeGames
                        |> List.head
            in
            case findGame of
                Just (player1, player2, _) ->
                    let
                        opponent =
                            if player1 == clientId then
                                player2
                            else
                                player1
                                
                        updatedGames =
                            List.filter
                                (\(p1, p2, _) ->
                                    not (p1 == player1 && p2 == player2)
                                )
                                model.activeGames
                    in
                    ( { model | activeGames = updatedGames }
                    , sendToFrontend opponent OpponentLeft
                    )
                
                Nothing ->
                    ( model, Cmd.none )
            
        MakeMove boardIndex cellIndex player ->
            let
                findGame =
                    List.filter
                        (\(player1, player2, _) ->
                            player1 == clientId || player2 == clientId
                        )
                        model.activeGames
                        |> List.head
            in
            case findGame of
                Just (player1, player2, _) ->
                    let
                        opponent =
                            if player1 == clientId then
                                player2
                            else
                                player1
                    in
                    ( model
                    , sendToFrontend opponent (OpponentMove { boardIndex = boardIndex, cellIndex = cellIndex, player = player })
                    )
                
                Nothing ->
                    ( model, Cmd.none )
