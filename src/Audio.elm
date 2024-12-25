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
import LocalStorage exposing (LocalStorage)
import Types exposing (Player(..))


port playSound_ : E.Value -> Cmd msg


playButtonClick : LocalStorage -> Command FrontendOnly toMsg msg
playButtonClick localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "button-click")

    else
        Command.none


playWinSound : LocalStorage -> Command FrontendOnly toMsg msg
playWinSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "win")

    else
        Command.none


playLoseSound : LocalStorage -> Command FrontendOnly toMsg msg
playLoseSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "lose")

    else
        Command.none


playDrawSound : LocalStorage -> Command FrontendOnly toMsg msg
playDrawSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "draw")

    else
        Command.none


playMoveSound : LocalStorage -> Player -> Command FrontendOnly toMsg msg
playMoveSound localStorage player =
    if localStorage.soundEnabled then
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

    else
        Command.none


playErrorSound : LocalStorage -> Command FrontendOnly toMsg msg
playErrorSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "error")

    else
        Command.none


playSmallWinSound : LocalStorage -> Command FrontendOnly toMsg msg
playSmallWinSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "small-win")

    else
        Command.none


playOnlineSound : LocalStorage -> Command FrontendOnly toMsg msg
playOnlineSound localStorage =
    if localStorage.soundEnabled then
        Command.sendToJs "playSound_" playSound_ (E.string "play-online")

    else
        Command.none
