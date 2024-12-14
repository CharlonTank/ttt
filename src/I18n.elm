module I18n exposing (Translation, translations, playerToString)

import Types exposing (Language(..), Player(..))


type alias Translation =
    { welcome : String
    , description : String
    , playWithFriend : String
    , playWithBot : String
    , chooseDifficulty : String
    , easy : String
    , medium : String
    , hard : String
    , elite : String
    , back : String
    , playerXTurn : String
    , playerOTurn : String
    , playerWins : String -> String
    , restart : String
    , backToMenu : String
    , playingWithFriend : String
    , playingWithBot : String -> String
    }


translations : Language -> Translation
translations lang =
    case lang of
        FR ->
            { welcome = "Ultimate Morpion"
            , description = "Bienvenue dans Ultimate Morpion ! Défiez votre réflexion stratégique dans cette version avancée du Morpion."
            , playWithFriend = "Jouer hors ligne avec un ami"
            , playWithBot = "Jouer avec un bot"
            , chooseDifficulty = "Choisir la difficulté"
            , easy = "Facile"
            , medium = "Moyen"
            , hard = "Difficile"
            , elite = "Élite"
            , back = "Retour"
            , playerXTurn = "Tour du joueur X"
            , playerOTurn = "Tour du joueur O"
            , playerWins = \player -> "🎉 " ++ player ++ " gagne ! 🎉"
            , restart = "Recommencer"
            , backToMenu = "Retour au menu"
            , playingWithFriend = "Mode: Hors ligne avec un ami"
            , playingWithBot = \difficulty -> "Mode: Contre le bot (" ++ difficulty ++ ")"
            }

        EN ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Welcome to Ultimate Tic Tac Toe! Challenge your strategic thinking in this advanced version of the game."
            , playWithFriend = "Play offline with a friend"
            , playWithBot = "Play with bot"
            , chooseDifficulty = "Choose difficulty"
            , easy = "Easy"
            , medium = "Medium"
            , hard = "Hard"
            , elite = "Elite"
            , back = "Back"
            , playerXTurn = "Player X's turn"
            , playerOTurn = "Player O's turn"
            , playerWins = \player -> "🎉 " ++ player ++ " wins! 🎉"
            , restart = "Restart"
            , backToMenu = "Back to menu"
            , playingWithFriend = "Mode: Playing with friend"
            , playingWithBot = \difficulty -> "Mode: Playing against bot (" ++ difficulty ++ ")"
            }


playerToString : Language -> Player -> String
playerToString lang player =
    case lang of
        FR ->
            case player of
                X ->
                    "Joueur X"
                O ->
                    "Joueur O"
        EN ->
            case player of
                X ->
                    "Player X"
                O ->
                    "Player O" 