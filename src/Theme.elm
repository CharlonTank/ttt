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


themes : ThemePreference -> Theme
themes preference =
    case preference of
        DarkMode ->
            darkTheme

        LightMode ->
            lightTheme

        SystemMode systemMode ->
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



type ThemePreference
    = DarkMode
    | LightMode
    | SystemMode SystemDarkOrLight


type SystemDarkOrLight
    = Dark
    | Light


stringToThemePreference : String -> ThemePreference
stringToThemePreference str =
    case str of
        "dark" ->
            DarkMode

        "light" ->
            LightMode

        "system-dark" ->
            SystemMode Dark

        "system-light" ->
            SystemMode Light

        _ ->
            -- Default if unrecognized
            SystemMode Light


themePreferenceToString : ThemePreference -> String
themePreferenceToString preference =
    case preference of
        DarkMode ->
            "dark"

        LightMode ->
            "light"

        SystemMode Dark ->
            "system-dark"

        SystemMode Light ->
            "system-light" 