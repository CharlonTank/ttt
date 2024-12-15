module I18n exposing (Translation, playerToString, translations)

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
    , playForMe : String
    , rules : String
    , rulesTitle : String
    , rulesText : String
    , close : String
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
            , playForMe = "Jouer à ma place"
            , rules = "Règles du jeu"
            , rulesTitle = "Comment jouer"
            , rulesText = """Le Ultimate Tic Tac Toe est une version avancée du morpion classique. Voici les règles :

1. Le plateau est composé de 9 petits morpions.
2. À votre tour, vous devez jouer dans le petit morpion correspondant à la case où votre adversaire a joué.
3. Si vous êtes envoyé dans un morpion déjà gagné ou plein, vous pouvez jouer dans n'importe quel morpion disponible.
4. Pour gagner, vous devez aligner 3 morpions gagnés (horizontalement, verticalement ou en diagonale).
5. Les cases gagnées par X sont marquées en rouge, celles gagnées par O en bleu."""
            , close = "Fermer"
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
            , playForMe = "Play for me"
            , rules = "Game Rules"
            , rulesTitle = "How to Play"
            , rulesText = """Ultimate Tic Tac Toe is an advanced version of the classic game. Here are the rules:

1. The board consists of 9 small tic-tac-toe boards.
2. On your turn, you must play in the small board corresponding to the cell where your opponent played.
3. If you are sent to a board that is already won or full, you can play in any available board.
4. To win, you must align 3 won boards (horizontally, vertically, or diagonally).
5. Cells won by X are marked in red, those won by O in blue."""
            , close = "Close"
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
