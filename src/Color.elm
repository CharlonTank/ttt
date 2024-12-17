module Color exposing
    ( danger
    , darkBackground
    , darkBorder
    , darkGradientBackground
    , darkSecondaryBackground
    , darkText
    , darkTextHover
    , disabled
    , getBackground
    , getBorder
    , getSecondaryBackground
    , lightBackground
    , lightBorder
    , lightGradientBackground
    , lightSecondaryBackground
    , lightText
    , playerO
    , playerX
    , primary
    , success
    , withAlpha
    )

-- Dark Mode Colors


darkBackground : String
darkBackground =
    "#1a202c"


darkText : String
darkText =
    "#e2e8f0"


darkSecondaryBackground : String
darkSecondaryBackground =
    "#2d3748"


darkBorder : String
darkBorder =
    "#4a5568"


darkTextHover : String
darkTextHover =
    "#f1f5f9"


darkGradientBackground : String
darkGradientBackground =
    "linear-gradient(135deg, #111111 0%, #1a1f24 100%)"



-- Light Mode Colors


lightBackground : String
lightBackground =
    "white"


lightText : String
lightText =
    "#2c3e50"


lightSecondaryBackground : String
lightSecondaryBackground =
    "#f8f9fa"


lightBorder : String
lightBorder =
    "#e2e8f0"


lightGradientBackground : String
lightGradientBackground =
    "linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%)"



-- Brand Colors


primary : String
primary =
    "#3498db"


danger : String
danger =
    "#e74c3c"


success : String
success =
    "#4CAF50"


disabled : String
disabled =
    "#bdc3c7"



-- Game Colors


playerX : String
playerX =
    "#ff8a65"


playerO : String
playerO =
    "#64b5f6"



-- Helper functions


getBackground : Bool -> String
getBackground isDark =
    if isDark then
        darkBackground

    else
        lightBackground


getSecondaryBackground : Bool -> String
getSecondaryBackground isDark =
    if isDark then
        darkSecondaryBackground

    else
        lightSecondaryBackground


getBorder : Bool -> String
getBorder isDark =
    if isDark then
        darkBorder

    else
        lightBorder



-- Opacity helpers


withAlpha : String -> Float -> String
withAlpha color alpha =
    let
        hexToRgb hex =
            let
                r =
                    String.slice 1 3 hex |> hexToInt

                g =
                    String.slice 3 5 hex |> hexToInt

                b =
                    String.slice 5 7 hex |> hexToInt
            in
            ( r, g, b )

        hexToInt hex =
            String.toInt ("0x" ++ hex) |> Maybe.withDefault 0
    in
    if String.startsWith "#" color then
        let
            ( r, g, b ) =
                hexToRgb color
        in
        "rgba("
            ++ String.fromInt r
            ++ ", "
            ++ String.fromInt g
            ++ ", "
            ++ String.fromInt b
            ++ ", "
            ++ String.fromFloat alpha
            ++ ")"

    else
        color
