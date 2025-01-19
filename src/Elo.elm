module Elo exposing
    ( calculateNewRating
    , expectedScore
    , getEloRating
    , getEloRatingFromDifficulty
    , kFactor
    , updateEloRating
    , updateEloRatings
    )

{-| Calculate the expected score for a player based on their rating and their opponent's rating.
The expected score is a number between 0 and 1 representing the probability of winning.
-}

import Types exposing (BotDifficulty(..), Player(..))


expectedScore : Int -> Int -> Float
expectedScore playerRating opponentRating =
    1 / (1 + (10 ^ (toFloat (opponentRating - playerRating) / 400)))


{-| K-factor determines how much a single game can affect the rating.
Lower rated players and newer players should have a higher K-factor
to allow their ratings to adjust more quickly.
-}
kFactor : Int -> Int
kFactor currentRating =
    if currentRating < 1000 then
        32

    else if currentRating < 2000 then
        24

    else
        16


{-| Calculate the new rating for a player based on:

  - Their current rating
  - Their opponent's rating
  - The actual score (1 for win, 0.5 for draw, 0 for loss)

-}
calculateNewRating : Int -> Int -> Float -> Int
calculateNewRating currentRating opponentRating actualScore =
    let
        expected =
            expectedScore currentRating opponentRating

        k =
            kFactor currentRating
    in
    round (toFloat currentRating + toFloat k * (actualScore - expected))


{-| Update both players' Elo ratings after a game.
Returns a tuple of (winner's new rating, loser's new rating).
For a draw, both players are considered winners with a score of 0.5.
-}
updateEloRatings :
    { winner : Int
    , loser : Int
    , isDraw : Bool
    }
    -> ( Int, Int )
updateEloRatings { winner, loser, isDraw } =
    if isDraw then
        ( calculateNewRating winner loser 0.5
        , calculateNewRating loser winner 0.5
        )

    else
        ( calculateNewRating winner loser 1.0
        , calculateNewRating loser winner 0.0
        )


getEloRating : Player -> Int
getEloRating player =
    case player of
        Authenticated publicUser ->
            publicUser.elo

        Anonymous _ elo ->
            elo


getEloRatingFromDifficulty : BotDifficulty -> Int
getEloRatingFromDifficulty difficulty =
    case difficulty of
        Easy ->
            600

        Medium ->
            800

        Hard ->
            1000

        Elite ->
            1200


updateEloRating : Player -> Int -> Player
updateEloRating player elo =
    case player of
        Authenticated publicUser ->
            Authenticated { publicUser | elo = elo }

        Anonymous sessionId _ ->
            Anonymous sessionId elo
