port module Audio exposing
    ( Sound(..)
    , playSound
    )

import Effect.Command as Command exposing (Command, FrontendOnly)
import Json.Encode as E
import Types exposing (PlayerSide(..))


port playSound_ : E.Value -> Cmd msg


type Sound
    = ButtonClickSound
    | WinSound
    | LoseSound
    | DrawSound
    | MoveSound PlayerSide
    | ErrorSound
    | SmallWinSound
    | PlayOnlineSound


soundToString : Sound -> String
soundToString sound =
    case sound of
        ButtonClickSound ->
            "button-click"

        WinSound ->
            "win"

        LoseSound ->
            "lose"

        DrawSound ->
            "draw"

        MoveSound player ->
            case player of
                X ->
                    "move-x"

                O ->
                    "move-o"

        ErrorSound ->
            "error"

        SmallWinSound ->
            "small-win"

        PlayOnlineSound ->
            "play-online"


playSound : Sound -> Command FrontendOnly toMsg msg
playSound sound =
    Command.sendToJs "playSound_" playSound_ (E.string (soundToString sound))
