module Theme exposing (..)

import Color


type alias Theme =
    { background : String
    , gradientBackground : String
    , secondaryBackground : String
    , tertiaryBackground : String
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


type UserPreference
    = DarkMode
    | LightMode
    | SystemMode


type Mode
    = Dark
    | Light


themes : UserPreference -> Mode -> Theme
themes preference systemMode =
    case preference of
        DarkMode ->
            darkTheme

        LightMode ->
            lightTheme

        SystemMode ->
            case systemMode of
                Dark ->
                    darkTheme

                Light ->
                    lightTheme


darkTheme : Theme
darkTheme =
    { background = Color.darkBackground
    , gradientBackground = Color.darkGradientBackground
    , secondaryBackground = Color.darkSecondaryBackground
    , tertiaryBackground = Color.darkSecondaryBackground
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


lightTheme : Theme
lightTheme =
    { background = Color.lightBackground
    , gradientBackground = Color.lightGradientBackground
    , secondaryBackground = Color.primary
    , tertiaryBackground = Color.lightSecondaryBackground
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


stringToUserPreference : String -> UserPreference
stringToUserPreference str =
    case str of
        "dark" ->
            DarkMode

        "light" ->
            LightMode

        "system-dark" ->
            SystemMode

        "system-light" ->
            SystemMode

        _ ->
            -- Default if unrecognized
            SystemMode


userPreferenceToString : UserPreference -> Mode -> String
userPreferenceToString preference systemMode =
    case preference of
        DarkMode ->
            "dark"

        LightMode ->
            "light"

        SystemMode ->
            case systemMode of
                Dark ->
                    "system-dark"

                Light ->
                    "system-light"
