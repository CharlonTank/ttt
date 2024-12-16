module I18n exposing (Translation, playerToString, translations)

import Types exposing (Language(..), Player(..))


type alias Translation =
    { welcome : String
    , description : String
    , playWithFriend : String
    , playWithBot : String
    , playOnline : String
    , searching : String
    , chooseDifficulty : String
    , easy : String
    , medium : String
    , hard : String
    , elite : String
    , back : String
    , backToMenu : String
    , playingWithFriend : String
    , playingWithBot : String -> String
    , playingOnline : String
    , humanStarts : String
    , botStarts : String
    , randomStarts : String
    , playForMe : String
    , playerXTurn : String
    , playerOTurn : String
    , playerWins : String -> String
    , rulesTitle : String
    , rulesText : String
    , close : String
    , yourTurn : String
    , enemyTurn : String
    , waitingForOpponent : String
    , abandon : String
    , confirmAbandon : String
    , cancelAbandon : String
    , draw : String
    , youWon : String
    , youLost : String
    }


translations : Language -> Translation
translations lang =
    case lang of
        FR ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Bienvenue dans Ultimate Tic Tac Toe ! Défiez votre réflexion stratégique dans cette version avancée du jeu."
            , playWithFriend = "Jouer hors ligne avec un ami"
            , playWithBot = "Jouer contre l'ordinateur"
            , playOnline = "Jouer en ligne"
            , searching = "Recherche d'un adversaire..."
            , chooseDifficulty = "Choisissez la difficulté"
            , easy = "Facile"
            , medium = "Moyen"
            , hard = "Difficile"
            , elite = "Elite"
            , back = "Retour"
            , backToMenu = "Retour au menu"
            , playingWithFriend = "Mode : Jeu avec un ami"
            , playingWithBot = \difficulty -> "Mode : Jeu contre l'ordinateur (" ++ difficulty ++ ")"
            , playingOnline = "Mode : Jeu en ligne"
            , humanStarts = "Vous commencez"
            , botStarts = "L'ordinateur commence"
            , randomStarts = "🎲"
            , playForMe = "Jouer pour moi"
            , playerXTurn = "Tour du joueur X"
            , playerOTurn = "Tour du joueur O"
            , playerWins = \player -> player ++ " gagne !"
            , rulesTitle = "Règles du jeu"
            , rulesText = "Le Ultimate Tic Tac Toe est une version avancée du morpion classique.\n\nLe plateau est composé de 9 petits morpions. Pour gagner, vous devez remporter 3 petits morpions alignés (horizontalement, verticalement ou en diagonale).\n\nQuand vous jouez dans une case d'un petit morpion, votre adversaire doit jouer son prochain coup dans le petit morpion correspondant. Par exemple, si vous jouez dans la case en haut à droite d'un petit morpion, votre adversaire doit jouer dans le petit morpion en haut à droite.\n\nSi vous envoyez votre adversaire dans un morpion déjà gagné ou plein (match nul), il pourra jouer dans n'importe quel petit morpion disponible.\n\nPour gagner un petit morpion, alignez 3 de vos symboles dans ce morpion, comme dans un morpion classique."
            , close = "Fermer"
            , yourTurn = "C'est votre tour !"
            , enemyTurn = "Tour de l'adversaire..."
            , waitingForOpponent = "En attente d'un adversaire..."
            , abandon = "Abandonner"
            , confirmAbandon = "Confirmer l'abandon"
            , cancelAbandon = "Annuler"
            , draw = "Match nul !"
            , youWon = "Vous avez gagné ! 🎉"
            , youLost = "Vous avez perdu ! 😢"
            }

        EN ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Welcome to Ultimate Tic Tac Toe! Challenge your strategic thinking in this advanced version of the game."
            , playWithFriend = "Play offline with a friend"
            , playWithBot = "Play with bot"
            , playOnline = "Play online"
            , searching = "Searching for opponent..."
            , chooseDifficulty = "Choose difficulty"
            , easy = "Easy"
            , medium = "Medium"
            , hard = "Hard"
            , elite = "Elite"
            , back = "Back"
            , backToMenu = "Back to menu"
            , playingWithFriend = "Mode: Playing with friend"
            , playingWithBot = \difficulty -> "Mode: Playing against bot (" ++ difficulty ++ ")"
            , playingOnline = "Mode: Playing online"
            , humanStarts = "You start"
            , botStarts = "Bot starts"
            , randomStarts = "🎲"
            , playForMe = "Play for me"
            , playerXTurn = "Player X's turn"
            , playerOTurn = "Player O's turn"
            , playerWins = \player -> player ++ " wins!"
            , rulesTitle = "Game Rules"
            , rulesText = "Ultimate Tic Tac Toe is an advanced version of the classic game.\n\nThe board consists of 9 small tic tac toe boards. To win, you need to win 3 small boards in a line (horizontally, vertically, or diagonally).\n\nWhen you play in a cell of a small board, your opponent must play their next move in the corresponding small board. For example, if you play in the top-right cell of any small board, your opponent must play in the top-right small board.\n\nIf you send your opponent to a board that's already won or full (drawn), they can play in any available small board.\n\nTo win a small board, get 3 of your marks in a line within that board, just like in regular Tic Tac Toe."
            , close = "Close"
            , yourTurn = "It's your turn!"
            , enemyTurn = "Enemy's turn..."
            , waitingForOpponent = "Waiting for opponent..."
            , abandon = "Forfeit"
            , confirmAbandon = "Confirm forfeit"
            , cancelAbandon = "Cancel"
            , draw = "It's a draw!"
            , youWon = "You won! 🎉"
            , youLost = "You lost! 😢"
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
