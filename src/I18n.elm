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
    , start : String
    , backToMenu : String
    , playingWithFriend : String
    , playingWithBot : String -> String
    , humanStarts : String
    , botStarts : String
    }


translations : Language -> Translation
translations lang =
    case lang of
        FR ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Bienvenue dans Ultimate Tic Tac Toe ! Défiez votre réflexion stratégique dans cette version avancée du jeu."
            , playWithFriend = "Jouer avec un ami"
            , playWithBot = "Jouer avec le bot"
            , chooseDifficulty = "Choisissez la difficulté"
            , easy = "Facile"
            , medium = "Moyen"
            , hard = "Difficile"
            , elite = "Élite"
            , back = "Retour"
            , playerXTurn = "Tour du joueur X"
            , playerOTurn = "Tour du joueur O"
            , playerWins = \player -> "🎉 " ++ player ++ " gagne ! 🎉"
            , restart = "Recommencer"
            , start = "Commencer"
            , backToMenu = "Retour au menu"
            , playingWithFriend = "Mode : Jeu avec un ami"
            , playingWithBot = \difficulty -> "Mode : Jeu contre le bot (" ++ difficulty ++ ")"
            , humanStarts = "Vous commencez"
            , botStarts = "Le bot commence"
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
            , start = "Start"
            , backToMenu = "Back to menu"
            , playingWithFriend = "Mode: Playing with friend"
            , playingWithBot = \difficulty -> "Mode: Playing against bot (" ++ difficulty ++ ")"
            , humanStarts = "You start"
            , botStarts = "Bot starts"
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