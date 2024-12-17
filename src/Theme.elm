module Theme exposing (DarkOrLight(..), Theme, darkModeToString, stringToDarkOrLight, themes)

import Color


type alias Theme =
    { background : String
    , gradientBackground : String
    , secondaryBackground : String
    , text : String
    , primary : String
    , border : String
    , textHover : String
    , danger : String
    , success : String
    , disabled : String
    , playerX : String
    , playerO : String
    }


type DarkOrLight
    = Dark
    | Light


darkModeToString : DarkOrLight -> String
darkModeToString darkOrLight =
    case darkOrLight of
        Dark ->
            "true"

        Light ->
            "false"


stringToDarkOrLight : Bool -> DarkOrLight
stringToDarkOrLight bool =
    if bool then
        Dark

    else
        Light


themes : DarkOrLight -> Theme
themes darkOrLight =
    case darkOrLight of
        Dark ->
            { background = Color.darkBackground
            , gradientBackground = Color.darkGradientBackground
            , secondaryBackground = Color.darkSecondaryBackground
            , text = Color.darkText
            , primary = Color.primary
            , border = Color.darkBorder
            , textHover = Color.darkTextHover
            , danger = Color.danger
            , success = Color.success
            , disabled = Color.disabled
            , playerX = Color.playerX
            , playerO = Color.playerO
            }

        Light ->
            { background = Color.lightBackground
            , gradientBackground = Color.lightGradientBackground
            , secondaryBackground = Color.lightSecondaryBackground
            , text = Color.lightText
            , primary = Color.primary
            , border = Color.lightBorder
            , textHover = Color.lightText
            , danger = Color.danger
            , success = Color.success
            , disabled = Color.disabled
            , playerX = Color.playerX
            , playerO = Color.playerO
            }
