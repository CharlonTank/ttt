module I18n exposing (Language(..), Translation, languageToString, stringToLanguage, translations)


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
    , playerXWins : String
    , playerOWins : String
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
    , startTutorial : String
    , nextStep : String
    , tutorialBasicMove : String
    , tutorialBoardSelection : String
    , tutorialWinningSmall : String
    , tutorialFreeChoice : String
    , tutorialWinningBig : String
    , tutorialComplete : String
    , tutorialTitle : String
    , playerX : String
    , playerO : String
    , playingTutorial : String
    }


translations : Language -> Translation
translations lang =
    case lang of
        FR ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Bienvenue dans Ultimate Tic Tac Toe ! Défiez votre réflexion stratégique dans cette version avancée du jeu."
            , playWithFriend = "Jouer hors ligne avec un ami"
            , playWithBot = "Jouer contre le bot"
            , playOnline = "Jouer en ligne"
            , playingTutorial = "Mode : Tutoriel"
            , searching = "Recherche d'un adversaire"
            , chooseDifficulty = "Choisissez la difficulté"
            , easy = "Facile"
            , medium = "Moyen"
            , hard = "Difficile"
            , elite = "Elite"
            , back = "Retour"
            , backToMenu = "Retour au menu"
            , playingWithFriend = "Mode : Jeu avec un ami"
            , playingWithBot = \difficulty -> "Mode : Jeu contre le bot (" ++ difficulty ++ ")"
            , playingOnline = "Mode : Jeu en ligne"
            , humanStarts = "Vous commencez"
            , botStarts = "Le bot commence"
            , randomStarts = "🎲"
            , playForMe = "Jouer pour moi"
            , playerXTurn = "Tour du joueur X"
            , playerOTurn = "Tour du joueur O"
            , playerXWins = "Joueur X gagne !"
            , playerOWins = "Joueur O gagne !"
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
            , startTutorial = "Commencer le tutoriel"
            , nextStep = "Suivant"
            , tutorialBasicMove = "Pour commencer, vous jouez avec les X. Placez votre symbole dans la case en haut à droite du morpion central. C'est là que tout commence !"
            , tutorialBoardSelection = "Excellent ! Voici la règle principale : votre adversaire doit jouer dans le petit morpion qui correspond à la position de votre dernier coup. Comme vous avez joué dans la case en haut à droite, votre adversaire devra jouer dans le petit morpion en haut à droite (en vert)."
            , tutorialWinningSmall = "Pour gagner un petit morpion, alignez trois symboles comme dans un morpion classique. Cliquez sur la case du milieu pour gagner ce petit morpion !"
            , tutorialFreeChoice = "Excellent ! Vous avez gagné le morpion central. Comme votre dernier coup était au centre et que ce morpion est maintenant terminé, votre adversaire peut jouer dans n'importe quel morpion non terminé !"
            , tutorialWinningBig = "Pour gagner la partie, vous devez gagner trois petits morpions alignés sur le grand plateau. "
            , tutorialComplete = "Félicitations ! Vous maîtrisez maintenant les règles de base de l'Ultimate Tic Tac Toe. Prêt à relever le défi ?"
            , tutorialTitle = "Mode : Tutoriel"
            , playerX = "Joueur X"
            , playerO = "Joueur O"
            }

        EN ->
            { welcome = "Ultimate Tic Tac Toe"
            , description = "Welcome to Ultimate Tic Tac Toe! Challenge your strategic thinking in this advanced version of the game."
            , playWithFriend = "Play offline with a friend"
            , playWithBot = "Play with bot"
            , playOnline = "Play online"
            , playingTutorial = "Mode: Tutorial"
            , searching = "Searching for opponent"
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
            , playerXWins = "Player X wins!"
            , playerOWins = "Player O wins!"
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
            , startTutorial = "Start Tutorial"
            , nextStep = "Next"
            , tutorialBasicMove = "To start, you play as X. Place your mark in the top-right cell of the center board. That's where it all begins!"
            , tutorialBoardSelection = "Excellent! Here's the main rule: your opponent must play in the small board that corresponds to the position of your last move. Since you played in the top-right cell, your opponent must play in the top-right small board (highlighted in green)."
            , tutorialWinningSmall = "To win a small board, get three marks in a line just like in regular Tic Tac Toe. Click the middle cell to win this small board!"
            , tutorialFreeChoice = "Excellent! You've won the center board. Since your last move was in the center and this board is now complete, your opponent can play in any unfinished board!"
            , tutorialWinningBig = "Well done! To win the game, you need to win three small boards in a line on the big board. Keep playing strategically!"
            , tutorialComplete = "Congratulations! You now master the basic rules of Ultimate Tic Tac Toe. Ready to take on the challenge?"
            , tutorialTitle = "Mode: Tutorial"
            , playerX = "Player X"
            , playerO = "Player O"
            }


type Language
    = FR
    | EN


languageToString : Language -> String
languageToString lang =
    case lang of
        FR ->
            "fr"

        EN ->
            "en"


stringToLanguage : String -> Language
stringToLanguage str =
    case str of
        "fr" ->
            FR

        "en" ->
            EN

        _ ->
            EN
