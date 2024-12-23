port module Audio exposing
    ( playButtonClick
    , playDrawSound
    , playErrorSound
    , playLoseSound
    , playMoveSound
    , playOnlineSound
    , playSmallWinSound
    , playWinSound
    )

import Effect.Command as Command exposing (Command, FrontendOnly)
import Json.Encode as E
import Types exposing (Player(..))


port playSound_ : E.Value -> Cmd msg


playButtonClick : Command FrontendOnly toMsg msg
playButtonClick =
    Command.sendToJs "playSound_" playSound_ (E.string "button-click")


playWinSound : Command FrontendOnly toMsg msg
playWinSound =
    Command.sendToJs "playSound_" playSound_ (E.string "win")


playLoseSound : Command FrontendOnly toMsg msg
playLoseSound =
    Command.sendToJs "playSound_" playSound_ (E.string "lose")


playDrawSound : Command FrontendOnly toMsg msg
playDrawSound =
    Command.sendToJs "playSound_" playSound_ (E.string "draw")


playMoveSound : Player -> Command FrontendOnly toMsg msg
playMoveSound player =
    Command.sendToJs "playSound_"
        playSound_
        (E.string
            (case player of
                X ->
                    "move-x"

                O ->
                    "move-o"
            )
        )


playErrorSound : Command FrontendOnly toMsg msg
playErrorSound =
    Command.sendToJs "playSound_" playSound_ (E.string "error")


playSmallWinSound : Command FrontendOnly toMsg msg
playSmallWinSound =
    Command.sendToJs "playSound_" playSound_ (E.string "small-win")


playOnlineSound : Command FrontendOnly toMsg msg
playOnlineSound =
    Command.sendToJs "playSound_" playSound_ (E.string "play-online")
