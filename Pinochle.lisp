;************************************************************
;* Name:  Nitesh Parajuli                                   *
;* Project:  Project 2 - Pinochle (LISP)                    *
;* Class:  CMPS 366 OPL                                     *
;* Date:  10/20/2020                                        *
;************************************************************


;/* *********************************************

; Source code for the main entry point of the program

;********************************************* */

;/* *********************************************************************
;Function Name: Pinochle
;Purpose: Entry point of the game
;Parameters: None
;Return Value: beginGame - the main game loop function
;Local Variables: mainMenuInput - user's input for the menu displayed in the function
;Algorithm:
;         Take user's input and start a new game or load an existing game based on the input.
;Assistance Received: none
;********************************************************************* */
(DEFUN Pinochle ()
	(TERPRI)
  (PRINC " --------------------Pinochle-------------------- ")
  (TERPRI)
  (TERPRI)
  (PRINC "Please enter 1 to start a new game or 2 to load a saved game : ")
  	(LET*(
  			(mainMenuInput (inputValidation (READ) 1 2)))
  		(COND
  			((= mainMenuInput 1)
  				(beginGame (LIST 1) 0))
  			((= mainMenuInput 2)
  				(beginGame (loadGame) 1))
  		)))


;/* *********************************************************************
;Function Name: inputValidation
;Purpose: To validate user's input
;Parameters: 
;           input - value provided by user
;           begin - the lower bound of allowed values
;           end - the upper bound of allowed values
;Return Value: 
;            beginGame - a function that runs the game of pinochle
;Local Variables: 
;            mainMenuInput - user's input for the menu displayed in the function
;Algorithm:
;         1. Check if the value provided by the user is a list, not a number, out of bounds.
;           2. If yes, call inputValidation with a new input
;           3. If not, return input value
;Assistance Received: none
;********************************************************************* */

(DEFUN inputValidation(input begin end)
	(COND
		( (LISTP input)
			(WRITE-LINE "Invalid Input.")
			(LET ((userInput (READ)))
			(inputValidation userInput begin end)))
		( (NOT (NUMBERP input))
			(WRITE-LINE "Invalid Input.")
			(LET ((userInput (READ)))
			(inputValidation userInput begin end)))
		( (OR (< input begin) (> input end))
			(WRITE-LINE "Invalid Input.")
			(LET ((userInput (READ)))
			(inputValidation userInput begin end)))
		( t input)))


;/* *********************************************

;Source Code for setting up the game and starting a round

;********************************************* */



;/* *********************************************************************
;Function Name: beginGame
;Purpose: To setup the game and start a new round
;Parameters: 
;           game - a list containing all details of the game
;           option - an indication of the nature of the game (new, existing)
;Return Value: 
;             beginRound - a function that runs a round of the game
;Local Variables: 
;             mainMenuInput - user's input for the menu displayed in the function
;Algorithm:
;         1. Setup the round
;         2. Identify and set next player
;         3. Start the round
;Assistance Received: none
;********************************************************************* */

(DEFUN beginGame (game option)
  (COND
    ; New game.
    ((= option 0)
      (LET*(
          (newGame (setupRound game))
          (playerTurn (coinToss))
          (start (setNextPlayer newGame playerTurn)))
        (beginRound start playerTurn)))

    ;loaded game
    ((= option 1)
      (LET*( 
        (loadedGame (loadFromSavedGame game))
        (playerTurn (getNextPlayer loadedGame)))
      (beginRound loadedGame playerTurn)))

    ;additional rounds
    ((= option 2)
     (LET*( 
        (winner (getGameWinner game))
        (newGame (setupRound (LIST (+ (getRoundNumber game) 1))))
        (board (setComputerTotalScore newGame (getComputerTotalScore game)))
        (mGame (setHumanTotalScore board (getHumanTotalScore game))))
     (COND
        ((> (getHumanTotalScore game) (getComputerTotalScore game))
          (WRITE-LINE "Human Player has higher overall points and will go first for this round!")
          (beginRound (setNextPlayer mGame winner) 0))
        ((< (getHumanTotalScore game) (getComputerTotalScore game))
          (WRITE-LINE "Computer Player has higher overall points and will go first for this round!")
          (beginRound (setNextPlayer mGame winner) 1))
        (t
          (WRITE-LINE "Both players have equal total points. A coin toss will determine the first player for this round!")
          (LET
            ((playerTurn (coinToss)))
            (beginRound (setNextPlayer mGame playerTurn) playerTurn))))))))

;/* *********************************************************************
;Function Name: loadGame
;Purpose: To load game details from a saved file
;Parameters: None
;Return Value: 
;             fileDetails - a list containing all game details from a saved file
;Local Variables: 
;             in - file provided by the user
;Algorithm:
;         1. Ask user for a filename
;           2. If the file exists, open and read from the file
;           3. If not, show error message
;         4. Return the details read from file as a list
;Assistance Received: none
;********************************************************************* */
(DEFUN loadGame()
  (PRINC "Please enter the name of the saved file: ")
  (LET* (
    (in (OPEN (READ) :if-does-not-exist NIL))
    (fileDetails ( COND
                  (IN (READ IN))
              (t
                (WRITE-LINE "Cannot Open File.")
                (loadGame)))))
    (COND
      ((NULL IN)
        NIL)
      (t
        (CLOSE IN)))
    fileDetails))

;/* *********************************************************************
;Function Name: loadFromSavedGame
;Purpose: To process the details read from a saved file 
;Parameters: 
;           game - a list containing all game details from a saved file
;Return Value: 
;           list containing all details as expected by a round
;Local Variables: 
;            variables storing information loaded from the saved file
;            variables storing the processed value which are added to the return list
;Algorithm:
;         1. Store information from the file into respective local variables.
;         2. Process each variable into the game's setup form
;         3. Return a list containing all processed variable
;Assistance Received: none
;********************************************************************* */
(DEFUN loadFromSavedGame (game)
    (LET*(
      (round (FIRST game))
      (computerTotalScore (FIRST (REST game)))
      (computerRoundScore (FIRST (REST (REST game))))
      (computerHand (FIRST (REST (REST (REST game)))))
      (computerCapture (FIRST (REST (REST (REST (REST game))))))
      (computerMeld (FIRST (REST (REST (REST (REST (REST game)))))))
      (humanTotalScore (FIRST (REST (REST (REST (REST (REST (REST game))))))))
      (humanRoundScore (FIRST (REST (REST (REST (REST (REST (REST (REST game)))))))))
      (humanHand (FIRST (REST (REST (REST (REST (REST (REST (REST (REST game))))))))))
      (humanCapture (FIRST (REST (REST (REST (REST (REST (REST (REST (REST (REST game)))))))))))
      (humanMeld (FIRST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST game))))))))))))
      (trump (FIRST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST game)))))))))))))
      (stockPile (FIRST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST game))))))))))))))
      (nextPlayerName (FIRST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST (REST game)))))))))))))))
      (nextPlayerIndex (COND
                      ( (STRING-EQUAL nextPlayerName "Human")
                        0)
                      ( t
                        1)))
      (processedTrumpCard (COND
                ((eq (length (STRING trump)) 1 )
                (LIST (read-from-string (concatenate 'string "-" (string trump))) 1 ()))
              (t
                (LIST trump 1 ()))
            ))
      (processedComputerHand (loadPile computerHand processedTrumpCard ))
      (processedComputerCapture (loadPile computerCapture (cons trump computerHand) ))
      (processedComputerMeld (loadMeldPile computerMeld processedTrumpCard (APPEND computerCapture (cons trump computerHand))))
      (processedComputerMelds (loadMelds computerMeld processedTrumpCard (APPEND computerCapture (cons trump computerHand))))
      (computerAssignedCards (APPEND (splitMeld computerMeld) (APPEND computerCapture (cons trump computerHand))))
      (processedComputerPlayable (APPEND processedComputerHand processedComputerMeld))
      (processedHumanHand (loadPile humanHand computerAssignedCards))
      (processedHumanCapture (loadPile humanCapture (APPEND humanHand computerAssignedCards)))
      (processedHumanMeld (loadMeldPile humanMeld processedTrumpCard (APPEND humanCapture (APPEND humanHand computerAssignedCards))))
      (processedHumanMelds (loadMelds humanMeld processedTrumpCard (APPEND humanCapture (APPEND humanHand computerAssignedCards))))
      (humanAssignedCards (APPEND (splitMeld humanMeld) (APPEND humanCapture (APPEND humanHand computerAssignedCards)) ))
      (processedHumanPlayable (APPEND processedHumanHand processedHumanMeld))
      (processedStockPile (loadPile stockPile humanAssignedCards)))
    (APPEND
      (LIST round) 
      (LIST processedHumanPlayable processedHumanHand processedHumanMeld processedHumanCapture humanRoundScore humanTotalScore processedHumanMelds () )
      (LIST processedComputerPlayable processedComputerHand processedComputerMeld processedComputerCapture computerRoundScore computerTotalScore processedComputerMelds () )
      (LIST processedTrumpCard)
      (LIST processedStockPile)
      (LIST nextPlayerIndex))))

;/* *********************************************************************
;Function Name: setupRound
;Purpose: To initialize all components of a round 
;Parameters: 
;           game - a list containing round number
;Return Value: 
;           list containing all game details as expected by a round
;Local Variables: 
;            deck - deck of game cards
;            humanDone - list of cards dealt to human
;            computerDone - list of cards dealt to computer 
;            trumpCard - trump card of the round    
;Algorithm:
;         1. Create a new deck and shuffle it.
;         2. Distribute cards to human and computer as per game's rule
;         3. Initialize playable pile, hand pile, meld pile, capture pile, roundscore, totalscore, meldMap and latestMove for each player and append it to the game list
;         4. Append trumpcard, stock pile and next player index to the game list
;         5. Return game list
;Assistance Received: none
;********************************************************************* */

(DEFUN setupRound(game)
    (LET* ( 
          (deck (shuffleCards(createDeck 2)))
          (human1 (dealCard 4 deck))
          (computer1 (dealCard 4 (removeDealtCards 4 deck)))
          (human2 (APPEND human1 (dealCard 4 (removeDealtCards 8 deck))))
          (computer2 (APPEND computer1 (dealCard 4 (removeDealtCards 12 deck))))
          (humanDone (APPEND human2 (dealCard 4 (removeDealtCards 16 deck))))
          (computerDone (APPEND computer2 (dealCard 4 (removeDealtCards 20 deck))))
          (trumpCard (dealCard 1 (removeDealtCards 24 deck))))
      (APPEND
        ;Round number
        game
        ; Human's playable, hand, meld, capture, roundScore, totalScore, meldMap, latestMove
        (LIST humanDone humanDone () () 0  0 () ())
        ; Computer's playable, hand, meld, capture, roundScore, totalScore, meldMap, latestMove
        (LIST computerDone computerDone () () 0  0 () ()) 
        trumpCard
        ;stockpile
        (LIST (removeDealtCards 25 deck))
        ;next player                
        (LIST ()))))                                      




;/* *********************************************

;Source Code for all getters and setters

;********************************************* */

;/* *********************************************************************
;Setter Functions
;Purpose: To set the values of items in the game list
;Parameters: 
;           game - the list containing all details of the game
;           (2nd parameter): the value to be substituted in the list
;Return Value: 
;           game list after substituting the value
;Local Variables:  
;Algorithm:
;         1. For each setter function, subtitute the new value in the game list and return the updated game list.
;Assistance Received: none
;********************************************************************* */
;Round Number
(DEFUN setRoundNumber (game num)
  (SUBSTITUTE num (ELT game 0) game :start 0 :count 1))

; Human setter functions
(DEFUN setHumanPlayable (game pile)
  (SUBSTITUTE pile (ELT game 1) game :start 1 :count 1))

(DEFUN setHumanHand (game humanHand)
  (SUBSTITUTE humanHand (ELT game 2) game :start 2 :count 1))

(DEFUN setHumanMeld (game meld)
  (SUBSTITUTE meld (ELT game 3) game :start 3 :count 1))

(DEFUN setHumanCapture (game humanCapture)
  (SUBSTITUTE humanCapture (ELT game 4) game :start 4 :count 1))

(DEFUN setHumanRoundScore (game humanScore)
  (SUBSTITUTE humanScore (ELT game 5) game :start 5 :count 1))

(DEFUN setHumanTotalScore (game humanScore)
  (SUBSTITUTE humanScore (ELT game 6) game :start 6 :count 1))

(DEFUN setHumanDeclaredMeld (game meld)
  (SUBSTITUTE meld (ELT game 7) game :start 7 :count 1))

(DEFUN setHumanPlayed (game humanPlayed)
  (SUBSTITUTE humanPlayed (ELT game 8) game :start 8 :count 1))


; Computer Setter functions
(DEFUN setComputerPlayable (game pile)
  (SUBSTITUTE pile (ELT game 9) game :start 9 :count 1))

(DEFUN setComputerHand (game computerHand)
  (SUBSTITUTE computerHand (ELT game 10) game :start 10 :count 1))

(DEFUN setComputerMeld (game meld)
  (SUBSTITUTE meld (ELT game 11) game :start 11 :count 1))

(DEFUN setComputerCapture (game computerCapture)
  (SUBSTITUTE computerCapture (ELT game 12) game :start 12 :count 1))

(DEFUN setComputerRoundScore (game computerScore)
  (SUBSTITUTE computerScore (ELT game 13) game :start 13 :count 1))

(DEFUN setComputerTotalScore (game computerScore)
  (SUBSTITUTE computerScore (ELT game 14) game :start 14 :count 1))

(DEFUN setComputerDeclaredMeld (game meld)
  (SUBSTITUTE meld (ELT game 15) game :start 15 :count 1))

(DEFUN setComputerPlayed (game computerPlayed)
  (SUBSTITUTE computerPlayed (ELT game 16) game :start 16 :count 1))

; Other setters

(DEFUN setTrumpCard (game trump)
  (SUBSTITUTE trump (ELT game 17) game :start 17 :count 1))

(DEFUN setStockPile (game deck)
  (SUBSTITUTE deck (ELT game 18) game :start 18 :count 1))

(DEFUN setNextPlayer (game nextPlayer)
  (SUBSTITUTE nextPlayer (ELT game 19) game :count 1 :from-end t))


;/* *********************************************************************
;Getter Functions
;Purpose: To get the values of items in the game list
;Parameters: 
;           game - the list containing all details of the game
;Return Value: 
;           the item accessed in the list
;Local Variables:  
;Algorithm:
;         1. For each getter function, return the item accessed from the game list.
;Assistance Received: none
;********************************************************************* */
; Getter functions
(DEFUN getRoundNumber (game)
  (ELT game 0))

(DEFUN getHumanPlayable (game)
  (ELT game 1))

(DEFUN getHumanHand (game)
  (ELT game 2))

(DEFUN getHumanMeld (game)
  (ELT game 3))

(DEFUN getHumanCapture (game)
  (ELT game 4))

(DEFUN getHumanRoundScore (game)
  (ELT game 5))

(DEFUN getHumanTotalScore (game)
  (ELT game 6))

(DEFUN getHumanDeclaredMeld (game)
  (ELT game 7))

(DEFUN getHumanPlayed (game)
  (ELT game 8))

(DEFUN getComputerPlayable (game)
  (ELT game 9))

(DEFUN getComputerHand (game)
  (ELT game 10))

(DEFUN getComputerMeld (game)
  (ELT game 11))

(DEFUN getComputerCapture (game)
  (ELT game 12))

(DEFUN getComputerRoundScore (game)
  (ELT game 13))

(DEFUN getComputerTotalScore (game)
  (ELT game 14))

(DEFUN getComputerDeclaredMeld (game)
  (ELT game 15))

(DEFUN getComputerPlayed (game)
  (ELT game 16))

(DEFUN getTrumpCard (game)
  (ELT game 17))

(DEFUN getStockPile (game)
  (ELT game 18))

(DEFUN getNextPlayer (game)
  (ELT game 19))


;/* *********************************************

;Source Code for initializing a round

;********************************************* */


;/* *********************************************************************
;Function Name: createDeck
;Purpose: To create the deck of cards
;Parameters: 
;          deckSize - the number of cardDecks needed to build the game deck
;Return Value: 
;           a list contating all game cards
;Local Variables:  
;Algorithm:
;         1. Create a list containing all legit game card. Each card contains its face and suit value, index, and a list of meld it has been used for.
;         2. Repeat the process for deckSize times and return a list after appending the returned list to the previously created list.
;Assistance Received: none
;********************************************************************* */

(DEFUN createDeck(decksize)
  (COND 
    ((= decksize 0)
        ())
    (t
      (APPEND (LIST (list '9S decksize () ) (list '9H decksize () ) (list '9C decksize () ) (list '9D decksize () )  
                    (list 'XS decksize () ) (list 'XH decksize () ) (list 'XC decksize () ) (list 'XD decksize () )
                    (list 'JS decksize () ) (list 'JH decksize () ) (list 'JC decksize () ) (list 'JD decksize () )
                    (list 'QS decksize () ) (list 'QH decksize () ) (list 'QC decksize () ) (list 'QD decksize () )
                    (list 'KS decksize () ) (list 'KH decksize () ) (list 'KC decksize () ) (list 'KD decksize () )
                    (list 'AS decksize () ) (list 'AH decksize () ) (list 'AC decksize () ) (list 'AD decksize () ))
        (createDeck(- decksize 1))))))


;/* *********************************************************************
;Function Name: shuffleCards
;Purpose: To shuffle a deck of cards
;Parameters: 
;          deck- a list containing all cards
;Return Value: 
;           a list contating shuffled cards
;Local Variables:  
;Algorithm:
;         1. Randomly shuffle the deck and return the shuffled deck.
;Assistance Received: none
;********************************************************************* */

(DEFUN shuffleCards(deck)
  (COND
    ( (null deck)
      ())
    (t
     (LET*(
          (randState (MAKE-RANDOM-STATE t))
          (index (RANDOM (LENGTH deck) randState)))
          (CONS
            (ELT deck index)
            (shuffleCards (removeCard(+ index 1) deck)))))))

;/* *********************************************************************
;Function Name: removeCard
;Purpose: To remove a card from a pile
;Parameters: 
;          index - index of the card being removed
;          pile - the pile from which the card is being removed
;Return Value: 
;           the list of cards after removing the indexed card
;Local Variables:  
;Algorithm:
;         1. Go through all cards in the pile.
;         2. Remove the indexed card and return the remaining pile.
;Assistance Received: none
;********************************************************************* */

(DEFUN removeCard(index pile)
  (COND 
    ((< index 1)
      pile)
    ((= index 1)
      (REST pile))
    (t
      (CONS (FIRST pile) (removeCard (- index 1) (REST pile))))))

;/* *********************************************************************
;Function Name: dealCard
;Purpose: To deal cards from the deck
;Parameters: 
;          num - number of cards to be dealt
;          deck - the list from which the cards are dealt
;Return Value: 
;           a list containing the num cards from the top of the deck
;Local Variables:  
;Algorithm:
;         1. Access the top card of the deck and add it to a list.
;         2. Recall the function until the desired number of cards are dealt.
;Assistance Received: none
;********************************************************************* */
(DEFUN dealCard(num deck)
  (COND
    ((OR (< num 1) (= (LENGTH deck) 0))
      ())
    (t
      (CONS (FIRST deck) (dealCard (- num 1) (REST deck))))))

;/* *********************************************************************
;Function Name: removeDealtCards
;Purpose: To remove dealt cards from the deck
;Parameters: 
;          num - number of cards to be removed
;          deck - the list from which the dealt cards are removed
;Return Value: 
;           the deck after removing the dealt cards
;Local Variables:  
;Algorithm:
;         1. Repeat the function execution for num times. Each execution will remove one card from the deck.
;         2. Return the remainder of the deck after num executions.
;Assistance Received: none
;********************************************************************* */
(DEFUN removeDealtCards (num deck)
  (COND
    ((OR (< num 1) (= (LENGTH deck) 0))
      deck)
    (t
      (removeDealtCards (- num 1) (REST deck)))))


;/* *********************************************************************
;Function Name: coinToss
;Purpose: To perform a coin toss
;Parameters: 
;Return Value: 
;           the index of the player who wins the toss
;Local Variables:  
;           userChoice: toss choice value given by the user.
;Algorithm:
;         1. Generate a random number between 1 and 2
;         2. If the user makes the correct guess, return human's index.
;         3. Else, return computer's index.
;Assistance Received: none
;********************************************************************* */
(DEFUN coinToss()
	(PRINC "Please enter 1 for Heads and 2 for Tails: ")
	(LET* (
		(userChoice (inputValidation (READ) 1 2))
		(state (make-random-state t))
    (tossResult(+ 1 (random 2 state))))
	(PRINC "Toss Result: ")
  (PRINC tossResult)
  (TERPRI)
		(COND
  			((= userChoice tossResult)
  			(WRITE-LINE "Human Player won the toss. Please make the first move.")
  			0)
  			(t 
  			(WRITE-LINE "Computer Player won the toss. It will make the first move.")
  			1))))


;/* *********************************************

;Source code to start a round, play turns, get turn winner, and make necessary changes to all rquired card piles

;********************************************* */

;/* *********************************************************************
;Function Name: beginRound
;Purpose: To start and run a round of the game
;Parameters: 
;         game - list containing all game details
;         nextPlayer - player making the next move
;Return Value: 
;           endGame function if the round has ended and user wants to quit.
;           beginGame function if the round has ended and user wants to continue.
;           playTurn function if the round hasn't ended.
;Local Variables:  
;           humanScoreSet - game list after setting human total score
;           computerScoreSet - game list after setting computer total score
;           save - a value indicating if the game needs to be saved
;Algorithm:
;         1. Run a continuos loop until both players are out of cards.
;         2. For each loop iteration, allow players to play their turns.
;         3. If players are out of cards, end the round and ask user for interest in additional rounds.
;         4. If yes, start a new round.
;         5. If not, end the game.
;Assistance Received: none
;********************************************************************* */

(DEFUN beginRound(game nextPlayer)
  (COND
      ((AND (eq 0 (LENGTH (getComputerPlayable game))) (eq 0 (LENGTH (getHumanPlayable game))))
      (TERPRI)
      (WRITE-LINE "Round has ended!")
      (LET*(
            (humanScoreSet (setHumanTotalScore game (+ (getHumanTotalScore game) (getHumanRoundScore game))))
            (computerScoreSet (setComputerTotalScore humanScoreSet (+ (getComputerTotalScore humanScoreSet) (getComputerRoundScore humanScoreSet)))))
            (roundStats computerScoreSet)
            (PRINC "Do you want play another round? Enter 1 for yes and 2 for no: ")
            (COND
              (( = (inputValidation (READ) 1 2) 1)
               (beginGame computerScoreSet 2))
            (t
              (gameStats computerScoreSet)
              (endGame)))))
    (t
      (mainScreen game)
      (LET*(
          (save (saveGame game)))
      (COND 
        ((eq save 2)
        (playTurn game nextPlayer))
        (t
          (endGame)))))))

;/* *********************************************************************
;Function Name: mainScreen
;Purpose: To display game details in the console
;Parameters: 
;         game - list containing all game details
;Return Value: 
;Local Variables:  
;Algorithm:
;         1. Access all components of the game using accessors and display to the console.
;Assistance Received: none
;********************************************************************* */
(DEFUN mainScreen (game)
  (TERPRI)
  (PRINC "Round : ")
  (PRINC (getRoundNumber game))
  (TERPRI)
  (TERPRI)
  (WRITE-LINE "Human")
  (WRITE-LINE "----------")
  (PRINC "Score: ")
  (PRINC (getHumanRoundScore game))
  (TERPRI)
  (PRINC "Hand: ")
  (PRINC (printPile(getHumanHand game)))
  (TERPRI)
  (PRINC "Capture Pile: ")
  (PRINC (printPile(getHumanCapture game)))
  (TERPRI)
  (PRINC "Melds: ")
  (PRINC (printMeld(getHumanDeclaredMeld game) game))
  (TERPRI)
  (TERPRI)
  (WRITE-LINE "Computer")
  (WRITE-LINE "----------")
  (PRINC "Score: ")
  (PRINC (getComputerRoundScore game))
  (TERPRI)
  (PRINC "Hand: ")
  (PRINC (printPile(getComputerHand game)))
  (TERPRI)
  (PRINC "Capture Pile: ")
  (PRINC (printPile(getComputerCapture game)))
  (TERPRI)
  (PRINC "Melds: ")
  (PRINC (printMeld(getComputerDeclaredMeld game) game))
  (TERPRI)
  (TERPRI)
  (PRINC "Trump: ")
  (COND
    ((eq (length (getStockPile game)) 0)
      (PRINC (CHAR(STRING(FIRST (getTrumpCard game))) 1)))
    (t
      (WRITE (FIRST (getTrumpCard game)))))    
  (TERPRI)
  (PRINC "Stock Pile:")
  (PRINC (printPile(getStockPile game)))
  (TERPRI)
  (TERPRI))

;/* *********************************************************************
;Function Name: saveGame
;Purpose: To save the game to a file
;Parameters: 
;         game - list containing all game details
;Return Value: 
;         value indicating if the game was saved or not.
;Local Variables:  
;Algorithm:
;         1. Ask user if they want to save the game.
;         2. If yes, access all details and write it to a file and Return 1 indicating game was saved.
;         3. If not, Return 2 indicating game was not saved.
;Assistance Received: none
;********************************************************************* */
(DEFUN saveGame (game)
  (PRINC "Would you like to save the game? Enter 1 for yes and 2 for no: ")
  (LET*(
    (saveChoice(inputValidation (READ) 1 2)))
  (COND 
      ((eq saveChoice 1)
      (LET*(
          (nextPlayer (COND
                    ( (= (getNextPlayer game) 0)
                      (LIST "Human"))
                    ( t
                      (LIST "Computer"))))
          (saveFile (APPEND 
                      (LIST (getRoundNumber game)) 
                      (LIST (getComputerTotalScore game)) (LIST (getComputerRoundScore game)) (LIST (printPile  (getComputerHand game))) (printPile (LIST (getComputerCapture game))) (LIST (printMeld (getComputerDeclaredMeld game) game))
                      (LIST (getHumanTotalScore game)) (LIST (getHumanRoundScore game)) (LIST (printPile (getHumanHand game))) (LIST (printPile (getHumanCapture game))) (LIST (printMeld (getHumanDeclaredMeld game) game))
                      (LIST (FIRST (getTrumpCard game)))
                      (LIST (printPile (getStockPile game)))
                      nextPlayer)))
        (WITH-OPEN-FILE (OUTPUT "./savedGame.txt" :direction :output :if-exists :supersede)
                        (FORMAT OUTPUT "~a"  saveFile)))
        (WRITE-LINE "Game has been saved to savedGame.txt!")
        1)
      (t
        2))))

;/* *********************************************************************
;Function Name: playTurn
;Purpose: To proceed the turn based on player's choices
;Parameters: 
;         game - list containing all game details
;         nextPlayer - player playing the turn
;Return Value: 
;         playHand function if user wants to play a card
;         recommmended move if the user needs help
;         endGame if the user wants to quit
;Local Variables:  
;Algorithm:
;         1. Give turn choices options to the user
;         2. Call respective functions based on user's chocies.
;Assistance Received: none
;********************************************************************* */
(DEFUN playTurn (game nextPlayer)
  (COND
    ; Human turn
    ((= nextPlayer 0)
      (LET* (
            (userInput (getMenuInput nextPlayer)))
        (COND
            ((= userInput 1)
              (playHand game nextPlayer (getNextPlayer game)))
            ((= userInput 2)
                (COND 
                  ((eq (getNextPlayer game) 1 )
                    (LET*(
                          (recommendedCard(getRecommendedChaseCard (getHumanPlayable game) (getTrumpCard game) (getComputerPlayed game))))
                        (PRINC "I recommend you play ")
                        (PRINC (FIRST recommendedCard))
                        (COND
                          ((eq (getWinner (getComputerPlayed game) recommendedCard (getTrumpCard game) 1  ) 0)
                               (WRITE-LINE " because you can win the turn"))
                            (t
                              (WRITE-LINE " because you cannot win the turn")))
                        (TERPRI)
                        (playTurn game nextPlayer)))
                  (t
                    (LET*(
                          (recommendedCard(getRecommendedLeadCard (getHumanPlayable game) (getTrumpCard game))))
                        (PRINC "I recommend you play ")
                        (PRINC (FIRST recommendedCard))
                        (PRINC " because it is the strongest card in hand after taking possible melds into account.")
                        (TERPRI)
                        (playTurn game nextPlayer)))))
            ((= userInput 3)
              (endGame)))))

    ; Computer turn
    ((= nextPlayer 1)
      (LET* (
          (userInput (getMenuInput nextPlayer)))
        (COND
          ((= userInput 1)
            (playHand game nextPlayer (getNextPlayer game)))
          ((= userInput 2)
            (endGame)))))))

;/* *********************************************************************
;Function Name: getMenuInput
;Purpose: To present the choices and take input from players
;Parameters: 
;         nextPlayer - player playing the turn
;Return Value: 
;         user's choice from the available menu options
;Local Variables:  
;Algorithm: Display menu options and get user's choice.
;Assistance Received: none
;********************************************************************* */
(DEFUN getMenuInput (nextPlayer)
  (COND
    ((= nextPlayer 0)
      (TERPRI)
      (WRITE-LINE "Human's Turn")
      (WRITE-LINE "----------")
      (WRITE-LINE "1: Make a move.")
      (WRITE-LINE "2: I need help!")
      (WRITE-LINE "3: End game.")
      (WRITE-LINE "-----------------")
      (inputValidation (READ) 1 3))
    ((= nextPlayer 1)
      (TERPRI)
      (WRITE-LINE "Computer's Turn")
      (WRITE-LINE "----------")
      (WRITE-LINE "1: Process Computer's Move.")
      (WRITE-LINE "2: End game.")
      (WRITE-LINE "-----------------")
      (inputValidation (READ) 1 2))))

;/* *********************************************************************
;Function Name: playHand
;Purpose: To play a card for the turn
;Parameters: 
;         game - list containing all game details
;         nextPlayer - index of player playing the turn
;         lead - index of lead player
;Return Value: 
;         beginRound function call for subsequent turns 
;Local Variables:
;         computersPlayedCard or humansPlayedCard: card played by the human or computer
;         computerSetup or humanSetup: the game list after removing the played card from human's or computer's piles
;         removeFromMeld: game list after removing the card from playable pile, hand pile or meld pile, and adjusting the hand or meldpile if needed
;         winner: winner of the turn  
;Algorithm: 
;         1. Allow players to play a card. Remove the played card from all necessary piles and make changes accordingly.
;         2. If the player is lead player, allow other players to play.
;         3. If not, compare cards and get winner.
;         4. Allow the winner to declare melds.
;         5. Continue the round with subsequent turns 
;Assistance Received: none
;********************************************************************* */
(DEFUN playHand (game nextPlayer lead)
  (COND
    ; Computer play
    ((= nextPlayer 1)
      (COND 
        ((= lead 1)
          ;"Computer is lead"
          (LET*( 
              (computersPlayedCard ( getRecommendedLeadCard (getComputerPlayable game) (getTrumpCard game) ))
              (computerSetup (setComputerPlayable game (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerPlayable game) (getComputerPlayable game)) 1) (getComputerPlayable game))))
              (handIndex (getCardIndexInAPile computersPlayedCard (getComputerHand computerSetup) (getComputerHand computerSetup) ))
              (meldIndex (getCardIndexInAPile computersPlayedCard (getComputerMeld computerSetup) (getComputerMeld computerSetup) ))
              (computerSetup2 (setComputerPlayed computerSetup computersPlayedCard))
              (removeFromHand (setComputerHand computerSetup2 (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerHand computerSetup2) (getComputerHand computerSetup2)) 1) (getComputerHand computerSetup2))))
              (adjustHand (meldCardPlayed removeFromHand computersPlayedCard 1))
              (removeFromMeld (setComputerMeld adjustHand (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerMeld removeFromHand) (getComputerMeld removeFromHand)) 1) (getComputerMeld removeFromHand)))))
            (PRINC "Computer Played " )
            (PRINC (FIRST computersPlayedCard))
            (PRINC " because it is the strongest card in hand after taking possible melds into account.")
            (TERPRI)
            (COND
               ((AND  (eq lead 1)(eq nextPlayer 1))
                 (playTurn removeFromMeld 0 ))
               (t 
                 (LET*(
                      (leadCard (getLeadMove removeFromMeld 0))
                      (winner (getWinner leadCard computersPlayedCard (getTrumpCard removeFromMeld) 0)))
                    (COND
                      ((= winner 0)
                        (WRITE-LINE "Turn Winner: Human")
                        (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 0) 0))
                      (t  
                        (WRITE-LINE "Turn Winner: Computer")
                        (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 1) 1))))))))
        (t 
          ;"Computer is chase"
          (LET* ( 
                (computersPlayedCard ( getRecommendedChaseCard (getComputerPlayable game) (getTrumpCard game) (getHumanPlayed game)))
                (computerSetup (setComputerPlayable game (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerPlayable game) (getComputerPlayable game)) 1) (getComputerPlayable game))))
                (handIndex (getCardIndexInAPile computersPlayedCard (getComputerHand computerSetup) (getComputerHand computerSetup) ))
                (meldIndex (getCardIndexInAPile computersPlayedCard (getComputerMeld computerSetup) (getComputerMeld computerSetup) ))
                (computerSetup2 (setComputerPlayed computerSetup computersPlayedCard))
                (removeFromHand (setComputerHand computerSetup2 (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerHand computerSetup2) (getComputerHand computerSetup2)) 1) (getComputerHand computerSetup2))))
                (adjustHand (meldCardPlayed removeFromHand computersPlayedCard 1))
                (removeFromMeld (setComputerMeld adjustHand (removeCard (+ (getCardIndexInAPile  computersPlayedCard  (getComputerMeld removeFromHand) (getComputerMeld removeFromHand)) 1) (getComputerMeld removeFromHand)))))    
              (PRINC "Computer Played " )
              (PRINC (FIRST computersPlayedCard))
              (COND
                ((AND  (eq lead 1)(eq nextPlayer 1))
                 (playTurn removeFromMeld 0 ))               
               (t 
                 (LET*(
                      (leadCard (getLeadMove removeFromMeld 0))
                      (winner (getWinner leadCard computersPlayedCard (getTrumpCard removeFromMeld) 0)))
                    (COND
                      ((= winner 0)
                        (PRINC " because it could not win the turn")
                        (TERPRI)
                        (TERPRI)
                        (WRITE-LINE "Turn Winner: Human")
                        (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 0) 0))    
                      (t 
                        (PRINC " because it could win the turn")
                        (TERPRI) 
                        (TERPRI)
                        (WRITE-LINE "Turn Winner: Computer")    
                        (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 1) 1))))))))))
    ; Human play
    (t
      (LET*( 
          (humansPlayedCard (getUserCardChoice (getHumanHand game) (getHumanMeld game) 1 (getHumanHand game) (getHumanMeld game)))
          (humanSetup (setHumanPlayable game (removeCard (+ (getCardIndexInAPile  humansPlayedCard (getHumanPlayable game) (getHumanPlayable game)) 1) (getHumanPlayable game))))
          (handIndex (getCardIndexInAPile humansPlayedCard (getHumanHand humanSetup) (getHumanHand humanSetup) ))
          (meldIndex (getCardIndexInAPile humansPlayedCard (getHumanMeld humanSetup) (getHumanMeld humanSetup) ))
          (humanSetup2 (setHumanPlayed humanSetup humansPlayedCard))
          (removeFromHand (setHumanHand humanSetup2 (removeCard (+ (getCardIndexInAPile  humansPlayedCard (getHumanHand humanSetup2) (getHumanHand humanSetup2)) 1) (getHumanHand humanSetup2))))
          (adjustHand (meldCardPlayed removeFromHand humansPlayedCard 0))
          (removeFromMeld (setHumanMeld adjustHand (removeCard (+ (getCardIndexInAPile  humansPlayedCard (getHumanMeld adjustHand) (getHumanMeld adjustHand)) 1) (getHumanMeld adjustHand)))))
        (PRINC "Human Played : " )
        (PRINC (FIRST humansPlayedCard))
        (TERPRI)
        (COND
          ((AND (eq nextPlayer 0) (eq lead 0))
            (playTurn removeFromMeld 1))
          (t 
            (LET*(
                (leadCard (getLeadMove removeFromMeld 1))
                (winner (getWinner leadCard humansPlayedCard (getTrumpCard removeFromMeld) 1)))
              (COND
                ((= winner 0)
                  (TERPRI)
                  (WRITE-LINE "Turn Winner: Human") 
                  (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 0) 0))

                (t  
                  (TERPRI)
                  (WRITE-LINE "Turn Winner: Computer") 
                  (beginRound (setNextPlayer (setUpNextTurn removeFromMeld winner) 1)  1))))))))))

;/* *********************************************************************
;Function Name: playHand
;Purpose: To play a card for the turn
;Parameters: 
;         hand - moving hand pile. changes for each recursuve call
;         hand - moving meld pile. changes for each recursuve call
;         count - total number of cards available
;         originalHand - constant hand pile. does not change for each recursuve call
;         originalMeld - constant meld pile. does not change for each recursuve call
;Return Value: 
;         card played by user
;Local Variables:
;Algorithm: 
;       1. Display all cards from hand and meld pile.
;       2. Allow user's to choose a card and return the card.
;Assistance Received: none
;********************************************************************* */
(DEFUN getUserCardChoice (hand meld count originalHand originalMeld)
  (COND 
    ((= count 1)
      (WRITE-LINE "Please select the index of the card that you'd like to play: ")))
  (COND
    ((AND (= (length hand) 0) ( = (length meld) 0))
      (PRINC ": ")
      (COND 
        ((LET*(
            (userChoice (inputValidation (READ) 1 count)))
          (COND
            ((<= userChoice (length originalHand))
              (nth (- userChoice 1) originalHand))
            (t
              (nth (- (- userChoice (length originalHand)) 1) originalMeld)))))))
      ((> (length hand) 0)
        (COND
          ((= (length hand) (length originalHand))
          (PRINC "Hand: ")))
        (PRINC (FIRST (FIRST hand)))
        (PRINC "(")
        (PRINC count)
        (PRINC ")")
        (PRINC " ")
        (getUserCardChoice (REST hand) meld (+ count 1) originalHand originalMeld))
    (t
      (COND
        ((= (length meld) (length originalMeld))
        (PRINC "Meld: ")))
    (PRINC (FIRST (FIRST meld)))
    (PRINC "(")
    (PRINC count)
    (PRINC ")")
    (PRINC " ")
    (getUserCardChoice hand (REST meld) (+ count 1) originalHand originalMeld))))


;/* *********************************************************************
;Function Name: getLeadMove
;Purpose: To get the card played by lead player
;Parameters: 
;         game - list containing all game details
;         leadPlayer - index of lead player
;Return Value: 
;         card played by lead player
;Local Variables:
;Algorithm: Access the latestmove of the lead player and return.
;Assistance Received: none
;********************************************************************* */
(DEFUN getLeadMove(game leadPlayer)
  (COND
    ((= leadPlayer 0)
      (ELT game 8))
    (t 
      (ELT game 16))))

;/* *********************************************************************
;Function Name: getWinner
;Purpose: To get the winner of a turn
;Parameters: 
;         leadCard - lead card of the turn
;         chaseCard - chase card of the turn
;         trumpCard - trump card of the round
;         leadIndex - index of lead player
;Return Value: 
;         index of player who won the turn
;Local Variables:
;         leadCardPoints - points the lead card is worth
;         chaseCardPoints - points the chase card is worth
;Algorithm: Compare lead and chase cards and return the winner based on game rules.
;Assistance Received: none
;********************************************************************* */
(DEFUN getWinner(leadCard chaseCard trumpCard leadIndex)
  ; human is lead
  (LET* (
      (leadCardPoints (getCardPoints leadCard))
      (chaseCardPoints (getCardPoints chaseCard))
      (leadCardSuit (CHAR(STRING(FIRST leadCard)) 1))
      (chaseCardSuit (CHAR(STRING(FIRST chaseCard)) 1))
      (trumpCardSuit (CHAR(STRING(FIRST trumpCard)) 1))
      (chaseIndex  (COND
              ((= leadIndex 1)
                0)
              (t 1))))
  (COND
    ((eq leadCardSuit chaseCardSuit)
      (COND
        ((>= leadCardPoints chaseCardPoints )
          leadIndex)
        (t   chaseIndex)))
    (t 
      (COND
        ((eq chaseCardSuit trumpCardSuit)
          chaseIndex)
        (t  leadIndex))))))

;/* *********************************************************************
;Function Name: getCardPoints
;Purpose: To get the points of a card
;Parameters: 
;         card - the card whose points is needed
;Return Value: 
;         the points of the card
;Local Variables:
;         face- faceValue of the card
;Algorithm: Compare the face value of the card and return points accordingly.
;Assistance Received: none
;********************************************************************* */
(DEFUN getCardPoints (card)
  (LET*(
    (face (CHAR (STRING (FIRST card)) 0)))
    (COND
      ((eq face #\9)
        0)
      ((eq face #\J)
        2)
      ((eq face #\Q)
        3)
      ((eq face #\K)
        4)
      ((eq face #\X)
        10)
      ((eq face #\A)
        11))))

;/* *********************************************************************
;Function Name: getCardIndexInAPile
;Purpose: To get the index of a card in a pile
;Parameters: 
;         card - the card whose index is needed
;         pile - the moving pile. changes during each recursive call
;         originalPile - the constant pile. Remains constant for every recursive call
;Return Value: 
;         the index of the card
;Local Variables:
;Algorithm: 
;       Recursively go through all cards in the pile and return the index of the card.
;Assistance Received: none
;********************************************************************* */
(DEFUN getCardIndexInAPile(card pile originalPile)
    (COND
      ((= (length pile) 0)
        (- 0 (+ (length originalPile) 1)))
      ((= (checkCard card (FIRST pile)) 1)
        0)
      (t
        (+ (getCardIndexInAPile card (REST pile) originalPile) 1))))

;/* *********************************************************************
;Function Name: checkCard
;Purpose: To check if two cards are same
;Parameters: 
;         card1 - the first of the two cards
;         card2 - the second card
;Return Value: 
;         1 if the cards are same, 0 if not
;Local Variables:
;Algorithm: 
;       Compare the face and suit of the card and the index of the card. If they are equal, return 1 else return 0.
;Assistance Received: none
;********************************************************************* */
(DEFUN checkCard (card1 card2)
  (COND
    (( AND ( AND (EQ  (FIRST card1)   (FIRST card1) ) (EQ (FIRST card1)  (FIRST card2) )) 
            (EQ  (FIRST (REST card1))  (FIRST (REST card2))))
      1)
    ( t
      0)))

;/* *********************************************************************
;Function Name: meldCardPlayed
;Purpose: To check if a card from meld pile is played and return the game list after making changes.
;Parameters: 
;         game - list containing all game details
;         playedCard - card played by the player
;         player - index of the player
;Return Value: 
;         game list after necesarry changes
;Local Variables:
;         cardIndex - index of the played card in meldPile
;         removedPile - meldPile after the played card has been removed
;Algorithm: 
;       1. Check if played card is in meld pile.
;       2. If yes, remove the card from meld pile. Make adjustments to active melds and return the game list.
;       3. If not, return game list.
;Assistance Received: none
;********************************************************************* */
(DEFUN meldCardPlayed(game playedCard player)
  (COND
    ((eq player 0)
      (LET*(
         (cardIndex (getCardIndexInAPile  playedCard (getHumanMeld game) (getHumanMeld game)))
         (removedPile (removeCard (+ cardIndex 1) (getHumanMeld game))))
        (COND
          ((eq cardIndex -1)
             game)
          (t
             (LET*(
                (humanMeld (getHumanDeclaredMeld (setHumanMeld game removedPile)))
                (latest(getPushCards game humanMeld playedCard humanMeld player)))
              latest)))))
    (t
      (LET*(
         (cardIndex (getCardIndexInAPile  playedCard (getComputerMeld game) (getComputerMeld game)))
         (removedPile (removeCard (+ cardIndex 1) (getComputerMeld game))))
        (COND
          ((eq cardIndex -1)
             game)
           (t
             (LET*(
                (computerMeld (getComputerDeclaredMeld (setComputerMeld game removedPile)))
                (latest(getPushCards game computerMeld playedCard computerMeld player)))
              latest)))))))

;/* *********************************************************************
;Function Name: getPushCards
;Purpose: To get the cards that need to pushed from meld to hand pile
;Parameters: 
;         game - list containing all game details
;         meldMap - moving list containing all active melds and involved cards. changes for each recursive call.
;         card - card from meld pile played by the player
;         originalMeldMap - constant list containing all active melds and involved cards.
;Return Value: 
;         a list of cards that needs to be transferred to handPile
;Local Variables:
;         cardsToAdd - cards that need to be added to hand pile
;Algorithm: 
;       1. Check if the played card is a part of one of the active melds.
;       2. If yes, get all cards linked with the played card. Check if they are used for any other active melds, if yes leave them in meldpile. If not, push them to handPile
;       3. Repeat the process for all melds in meldMap.
;Assistance Received: none
;********************************************************************* */
(DEFUN getPushCards(game meldMap card originalMap player)
  (COND
   ((eq (LENGTH meldMap) 0)
    game)
  (t 
    (COND
      ((eq player 0)
        (COND
          ((eq (checkPlayedMeld (FIRST meldMap) card) 1)
            (LET*(
                (index(getMeldIndexInMeldMap card (FIRST meldMap) (FIRST meldMap) ))
                (removeIndexedCard (removeCard (+ index 1) (FIRST meldMap)))
                (cardsToAdd(getNewHandCards removeIndexedCard (getHumanMeld game) 0))
                (newHand (APPEND (getHumanHand game) cardsToAdd))
                (newMeld (getNewMeldCards cardsToAdd (getHumanMeld game)))
                (latestGame (setHumanDeclaredMeld game (removeCard (+ (getMeldPairNum (FIRST meldMap) originalMap originalMap) 1) (getHumanDeclaredMeld game)))))
              (getPushCards (setHumanMeld (setHumanHand latestGame newHand) newMeld ) (REST meldMap) card originalMap player)))
          (t
             (getPushCards game (REST meldMap) card originalMap player))))
      (t
        (COND
          ((eq (checkPlayedMeld (FIRST meldMap) card) 1)
            (LET*(
                (index(getMeldIndexInMeldMap card (FIRST meldMap) (FIRST meldMap) ))
                (removeIndexedCard (removeCard (+ index 1) (FIRST meldMap)))
                (cardsToAdd(getNewHandCards removeIndexedCard (getComputerMeld game) 0))
                (newHand (APPEND (getComputerHand game) cardsToAdd))
                (newMeld (getNewMeldCards cardsToAdd (getComputerMeld game)))
                (latestGame (setComputerDeclaredMeld game (removeCard (+ (getMeldPairNum (FIRST meldMap) originalMap originalMap) 1) (getComputerDeclaredMeld game)))))
              (getPushCards (setComputerMeld (setComputerHand latestGame newHand) newMeld ) (REST meldMap) card originalMap player)))
          (t
             (getPushCards game (REST meldMap) card originalMap player))))))))

;/* *********************************************************************
;Function Name: checkPlayedMeld
;Purpose: To check if a played card is in an active meld
;Parameters: 
;           cards - cards in the meld
;           card - card being checked
;Return Value: 
;           0 if the card is not found in the meld, 1 if it is found.
;Local Variables:
;Algorithm: 
;       1. Recursively check for the card in the meld. If card is found, return 1.
;Assistance Received: none
;********************************************************************* */
(DEFUN checkPlayedMeld (cards card)
  (COND 
    ((eq (LENGTH cards) 0)
      0)
    ((AND (eq (FIRST (FIRST cards)) (FIRST card)) (eq (FIRST (REST (FIRST cards))) (FIRST (REST card))))
      1)
    (t
      (checkPlayedMeld (REST cards) card))))

;/* *********************************************************************
;Function Name: getMeldIndexInMeldMap
;Purpose: To get the index of a meld in meldmap
;Parameters: 
;           meld - meld whose index is being determined
;           map - moving meld map. changes for each recursive call.
;           originalMap - constant meld map. constant for every recursive call.
;Return Value: 
;           index of the meld if found. -1 if not.
;Local Variables:
;Algorithm: 
;       1. Recursively check for the meld in meldMap. Return the index if found and return -1 if not
;Assistance Received: none
;********************************************************************* */
(DEFUN getMeldIndexInMeldMap (meld map originalMap)
  (COND
      ((= (length map) 0)
        (- 0 (+ (length originalMap) 1)))
      ((AND   (eq (FIRST meld) (FIRST (FIRST map)))
        (eq (FIRST (REST meld)) (FIRST (REST (FIRST map)))))
        0)
      (t
        (+ (getMeldIndexInMeldMap meld (REST map) originalMap) 1))))


;/* *********************************************************************
;Function Name: getNewHandCards
;Purpose: To get the list containing updated hand pile
;Parameters: 
;           pile - list of card to be added
;           meldPile - meld pile of the player
;           index - index indicating the iteration is being done on meldPile or the list of card to be added
;Return Value: 
;           a list contating updated hand cards
;Local Variables:
;Algorithm: 
;       1. Recursively check each card from pile. If it has been used for multiple melds, do not add it to hand.
;       2. If not, add it to hand.
;Assistance Received: none
;********************************************************************* */
(DEFUN getNewHandCards(pile meldPile index)
  (COND
    ((eq (length pile) 0)
     ())
    ((eq index 1)
      (COND
        ((AND (AND (eq (FIRST pile) (FIRST (FIRST meldPile))) (eq (FIRST (REST pile)) (FIRST (REST (FIRST meldPile))) ))
              (< (length (FIRST (REST (REST (FIRST meldPile))))) 2)) 
          (LIST (FIRST meldPile)))
        ((eq (length meldPile) 0)
          ())
        (t
           (getNewHandCards pile (REST meldPile) 1))))
    (t  
      (APPEND (getNewHandCards (FIRST pile) meldPile 1) (getNewHandCards (REST pile) meldPile 0)))))

;/* *********************************************************************
;Function Name: getNewMeldCards
;Purpose: To get the list containing updated meld pile
;Parameters: 
;           cards - list of card to be removed from meldPile
;           melds - list to be returned
;Return Value: 
;           a list contating updated meld cards
;Local Variables:
;Algorithm: 
;       1. Recursively check each card from cards and remove it from melds.
;       2. Return the list after removing all of such cards
;Assistance Received: none
;********************************************************************* */
(DEFUN getNewMeldCards(cards melds)
  (COND
    ((eq (length cards) 0)
      melds)
  (t 
    (getNewMeldCards (REST cards) (removeCard (+ (getCardIndexInAPile (FIRST cards) melds melds) 1) melds)))))

;/* *********************************************************************
;Function Name: getMeldPairNum
;Purpose: To get the index of a meld in meldMap
;Parameters: 
;           meldPair - meld whose index is being determined
;           map - moving meld map. changes for each recursive call.
;           originalMap - constant meld map. constant for every recursive call.
;Return Value: 
;           index of the meld if found. -1 if not
;Local Variables:
;Algorithm: 
;       1. Recursively check for the meld in meldMap. Return the index if found and return -1 if not
;Assistance Received: none
;********************************************************************* */
(DEFUN getMeldPairNum (meldPair map originalMap)
  (COND
      ((= (length map) 0)
        (- 0 (+ (length originalMap) 1))  )
      ((eq  meldPair (FIRST map))
        0)
      (t
        (+ (getMeldPairNum meldPair (REST map) originalMap) 1))))





;/* *********************************************

;Source code to play computer move and recommend human move for turns

;********************************************* */


;/* *********************************************************************
;Function Name: getRecommendedLeadCard
;Purpose: To get the best possible card for a lead move
;Parameters: 
;           playablePile - a list of all available cards for play
;           trumpCard - trumpCard of a round
;Return Value: 
;           best possible card for a lead move
;Local Variables:
;           pointsPerCard - a list containing total meldPoints remanining when a card is played
;           bestCard - the best lead card in the pile
;Algorithm: 
;       1. For each card in playable pile, get the points saved when each card is played.
;       2. The strongest card which saves the maximum meld points is returned as the best lead or recommended lead card.
;Assistance Received: none
;********************************************************************* */

(DEFUN getRecommendedLeadCard(playablePile trumpCard)
  (LET*(
      (pointsPerCard (leadCardAnalysis playablePile trumpCard playablePile))
      (bestCard (bestLeadCard pointsPerCard playablePile trumpCard -1 NIL)))
    bestCard))

;/* *********************************************************************
;Function Name: leadCardAnalysis
;Purpose: To analyze each card in playable pile and find the meld points saved when each card is played
;Parameters: 
;           playablePile - a moving list of all available cards for play. changes for each recursive call.
;           trumpCard - trumpCard of a round
;           originalPile - constant list of all available cards for play. Remains the same for all recursive call.
;Return Value: 
;           a list containing the points saved when each card is played
;Local Variables:
;           fiveCardCombo - a list of list of cards containing all possible combination of 1,2,4 and 5 cards
;           possibleMelds - all possible melds from the rest of the pile
;           totalMeldPoints - points saved when each card is played
;           referencedCard  - a list containing parameters for indicating the referenced card in generatecombo
;Algorithm: 
;       1. For each card in playablePile,
;         2. Create a list of cards from the playablePile containing the rest of the card in the pile
;         3. Retrieve all possible melds from the list and compute meld points.
;       2. Return the list of total meld points saved when each card is played.
;Assistance Received: none
;********************************************************************* */
(DEFUN leadCardAnalysis(playablePile trumpCard originalPile)
  (COND
    ((eq (length playablePile) 0)
      ())
    (t 
      (LET*(    
        (checkCards(removeCard (+ (getCardIndexInAPile (FIRST playablePile)  originalPile originalPile ) 1) originalPile))
        (referencedCard (addReferenceCard (length checkCards)))
        (oneCardCombo (generateAllCardsCombo () checkCards 1 0 0 referencedCard 0 ))
        (twoCardCombo (APPEND oneCardCombo (generateAllCardsCombo () checkCards 2 0 0 referencedCard 0 )))
        (fourCardCombo (APPEND twoCardCombo (generateAllCardsCombo () checkCards 4 0 0 referencedCard 0 )))
        (fiveCardCombo (APPEND fourCardCombo (generateAllCardsCombo () checkCards 5 0 0 referencedCard 0 )))
        (possiblemelds (getAllPossibleMelds fiveCardCombo trumpCard))
        (totalMeldPoints (getAllMeldPoints possiblemelds)))
      (cons totalMeldPoints (leadCardAnalysis (REST playablePile) trumpCard originalPile))))))

;/* *********************************************************************
;Function Name: addReferenceCard
;Purpose: To create a list of flags for each card for combination generation. 
;Parameters: 
;           size - the size of playablepile
;Return Value: 
;           a list containing the points saved when each card is played
;Local Variables:
;Algorithm: 
;       1. Create a list, of size elements, containing 0s indicating none of the card referenced before generating a combination.
;Assistance Received: none
;********************************************************************* */
(DEFUN addReferenceCard(size)
  (COND 
    ((eq size 0)
      ())
    (t
      (cons 0 (addReferenceCard (- size 1))))))

;/* *********************************************************************
;Function Name: generateAllCardsCombo
;Purpose: To generate a combination of cards of a specific length from a pile
;Parameters: 
;     combos - list storing all generated combinations
;     cards - pile of cards being used for the generating the combinations
;     requiredLength -  number of cards in the desired combination
;     startIndex - the card with reference to which the combination is being generated
;     currLength - the number of cards in the current combinaion
;     referencedCard - a utility vector to keep track of which card has been referenced for the combination
;     ret - an indication of which part of the function to execute
;Return Value: 
;           a list containing the points saved when each card is played
;Local Variables:
;Algorithm: 
;  1. check if current length is the desired length, if yes check which card has been referenced and push them in temp vector, and return.
;  2. if current length exceeds desired length, return.
;  3. if starting index is the last index in the pile, return.
;  4. recursively call the function with a next index and for next length
;  5. change the referencedCard value to indicate that the card is not longer the referenced card
;  6. recursively call the function by for next index
;Assistance Received: none
;********************************************************************* */
(DEFUN generateAllCardsCombo(combo cards reqLen startIndex currLen referencedCard ret)
  (COND 
    ((eq ret 2)
        combo)    

    (t (COND
        ((AND (> currLen reqLen) (eq ret 0))
          (generateAllCardsCombo combo cards reqLen startIndex currLen referencedCard 1 ))
        ((AND (eq currLen reqLen) (eq ret 0))
          (LET*(
            (temp(addToTempCombo cards referencedCard ())))
            (cons temp combo)))
        ((AND (eq startIndex (length cards))(eq ret 0))
            (generateAllCardsCombo combo cards reqLen startIndex currLen referencedCard 1 ))
        ((eq ret 0)
            (APPEND ( generateAllCardsCombo combo cards reqLen (+ startIndex 1) (+ currLen 1) (SUBSTITUTE 1 (ELT referencedCard startIndex) referencedCard :start startIndex :count 1) 0)
                    ( generateAllCardsCombo combo cards reqLen (+ startIndex 1) currLen  (SUBSTITUTE 0 (ELT referencedCard startIndex) referencedCard :start startIndex :count 1) 0)))
        (t 
          ( generateAllCardsCombo combo cards reqLen startIndex currLen  referencedCard 2))))))

;/* *********************************************************************
;Function Name: addToTempCombo
;Purpose: To add a new comination to a list
;Parameters: 
;     cards - list of cards containing the cards of the combination
;     referencedCard - a utility vector to keep track of which card is being referenced for the combination 
;     tempList - the final return list. updated on each recursive call
;Return Value: 
;           a list containing all referenced card
;Local Variables:
;Algorithm: 
;   1. For each card in cards, check if the referencedCard value of it is 1. 
;   2. If yes, add to templist and recursively call the function for rest of the card.
;   3. If not, do not add card to templist and recursively call the function for rest of the cards.
;   4. When the cards list is empty, return tempList.
;Assistance Received: none
;********************************************************************* */
(DEFUN addToTempCombo(cards referencedCard tempList)
  (COND
    ((eq (length cards) 0)
      tempList)
    (t
      (COND 
        ((eq (FIRST referencedCard) 1)
          (addToTempCombo (REST cards) (REST referencedCard) (cons (FIRST cards) tempList)))
        (t
          (addToTempCombo (REST cards) (REST referencedCard) tempList))))))

;/* *********************************************************************
;Function Name: bestLeadCard
;Purpose: To find the best card in the playablepile from the meld points saved list
;Parameters: 
;     pointsPerCard - a list containing total meldPoints remanining when a card is played
;     playablePile - list of available cards to play
;     trumpCard - trump card of the round
;     maxPoints - the maximum points in the pointsPerCard list
;     card - the best lead card in the pile. 
;Return Value: 
;           card  - the best lead card in the pile
;Local Variables:
;           thisCardPoint - points that the current card is worth
;           lastCardPoint - point that the best card so far is worth
;Algorithm: 
;       1. For each card in playable pile,
;          2. Check if the points saved is larger than maxPoints. If yes, set new maxPoints and card value and recursively call the function.
;          3. If not, check if it's equal. If yes, check which card is stronger among the two and set values for it. Recursively call the funciton
;          4. Else recursively call funciton with existing values.
;       5. Return card when the pile is empty. 
;Assistance Received: none
;********************************************************************* */
(DEFUN bestLeadCard(pointsPerCard playablePile trumpCard maxPoints card)
  (COND
    ((eq (length playablePile) 0)
      card)
    ((> (FIRST pointsPerCard) maxPoints)
      (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard (FIRST pointsPerCard) (FIRST playablePile) ))
    ((= (FIRST pointsPerCard) maxPoints)
      (COND
        ((AND (eq  (CHAR (STRING (FIRST (FIRST playablePile))) 1) (CHAR (STRING (FIRST trumpCard) ) 1)) (eq  (CHAR (STRING (FIRST card)) 1) (CHAR (STRING (FIRST trumpCard) ) 1)) )
          (LET*(
              (thisCardPoint (+ (getCardPoints (FIRST playablePile)) 100))
              (lastCardPoint (+ (getCardPoints card) 100)))
            (COND 
              ((> thisCardPoint lastCardPoint)
                (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard (FIRST pointsPerCard) (FIRST playablePile) ))
              (t
                (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard maxPoints card )))))
        ((eq (CHAR (STRING (FIRST (FIRST playablePile))) 1) (CHAR (STRING (FIRST trumpCard) ) 1))
          (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard (FIRST pointsPerCard) (FIRST playablePile) ))

        ((eq  (CHAR (STRING (FIRST card)) 1) (CHAR (STRING (FIRST trumpCard) ) 1))
          (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard maxPoints card ))
        (t
          (LET*(
              (thisCardPoint (getCardPoints (FIRST playablePile)))
              (lastCardPoint (getCardPoints card)))
            (COND 
              ((> thisCardPoint lastCardPoint)
                (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard (FIRST pointsPerCard) (FIRST playablePile) ))
              (t
                (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard maxPoints card )))))))
    (t
      (bestLeadCard (REST pointsPerCard) (REST playablePile) trumpCard maxPoints card ))))

;/* *********************************************************************
;Function Name: getRecommendedChaseCard
;Purpose: To get the best card for a chase move
;Parameters: 
;     playablePile - list of available cards to play
;     trumpCard - trump card of the round
;     leadCard - the maximum points in the pointsPerCard list
;Return Value: 
;           most inexpensive card that can win the turn or the most inexpensive card if cannot win the turn
;Local Variables:
;          winningCards - list of cards that can win the leadCard
;Algorithm: 
;         1. Get a list of cards from playable pile that can win the leadcard.
;         2. If list is empty, return lowest card from playable pile.
;         3. If not, return lowest card from the winningCards list.
;Assistance Received: none
;********************************************************************* */
(DEFUN getRecommendedChaseCard(playablePile trumpCard leadCard)
  (LET*(
      (winningCards (getWinningChaseCard playablePile trumpCard leadCard)))
  (COND
    ((eq (length winningCards) 0)
      (getMostInexpensiveCard playablePile trumpCard 200 NIL))  
    (t
      (getMostInexpensiveCard winningCards trumpCard 200 NIL)))))

;/* *********************************************************************
;Function Name: getWinningChaseCard
;Purpose: To get a list of cards from playable pile that can beat the lead card
;Parameters: 
;     playablePile - list of available cards to play
;     trumpCard - trump card of the round
;     leadCard - the maximum points in the pointsPerCard list
;Return Value: 
;           a list of cards that can beat the trump card
;Local Variables:
;Algorithm: 
;       1. For each card in playablepile, check if it can beat the lead card. If yes append the card to the return list.
;Assistance Received: none
;********************************************************************* */
(DEFUN getWinningChaseCard (playablePile trumpCard leadCard)
  (COND
    ((eq (length playablePile) 0)
      ())
    (t
      (COND
        ((eq (getWinner leadCard (FIRST playablePile) trumpCard 0) 1)
          (cons (FIRST playablePile) (getWinningChaseCard (REST playablePile) trumpCard leadCard)))
        (t
          (getWinningChaseCard (REST playablePile) trumpCard leadCard))))))

;/* *********************************************************************
;Function Name: getMostInexpensiveCard
;Purpose: To get the most Inexpensive card in a pile
;Parameters: 
;     pile - the pile of card 
;     trumpCard - trump card of the round
;     points - the points of the best card so far
;     card - best card so far
;Return Value: 
;           the least expensive card
;Local Variables:
;           thisCardPoint - points that the current card is worth
;Algorithm: 
;       1. For each card in pile, check if it's point is less than current lowest point.
;       2. If yes, set new lowest point and card and recursively call the function for other cards.
;       3. Else, recursively call the function with same points and card.
;       4. When pile is empty, return card.
;Assistance Received: none
;********************************************************************* */
(DEFUN getMostInexpensiveCard (pile trumpCard points card)
  (COND
    ((eq (length pile) 0)
      card)

    ((eq (CHAR (STRING (FIRST (FIRST pile))) 1) (CHAR (STRING (FIRST trumpCard) ) 1))
      (LET*(
          (thisCardPoint (+ (getCardPoints (FIRST pile)) 100)))
        (COND
          ((< thisCardPoint points)
            (getMostInexpensiveCard (REST pile) trumpCard thisCardPoint (FIRST pile)))
          (t
            (getMostInexpensiveCard (REST pile) trumpCard points card)))))
    (t
      (LET*(
          (thisCardPoint (getCardPoints (FIRST pile))))
        (COND
          ((< thisCardPoint points)
            (getMostInexpensiveCard (REST pile) trumpCard thisCardPoint (FIRST pile)))
          (t
            (getMostInexpensiveCard (REST pile) trumpCard points card)))))))


;/* *********************************************

;Source code to display piles and melds to console

;********************************************* */

;/* *********************************************************************
;Function Name: printPile
;Purpose: To print the face and suit of cards in a pile to the console
;Parameters: 
;     pile - the pile of card 
;Return Value: 
;           a list containing the face and suite of the cards in the pile
;Local Variables:
;Algorithm: 
;       1. For each card in pile, append it's facevalue and suit value to the return list
;Assistance Received: none
;********************************************************************* */
(DEFUN printPile (pile)
	(cond
		((null pile) ())
		(t 
      (cons (FIRST (FIRST pile)) (printPile (REST pile) )))))

;/* *********************************************************************
;Function Name: printMeld
;Purpose: To print the list containing all active melds
;Parameters: 
;     pile - the list of melds
;     game - the list containing all details of the turn
;Return Value: 
;           a list of all melds with card values set to console printable form
;Local Variables:
;Algorithm: 
;       1. For each meld in pile, print the card values of the meld. Repeat this execution for all melds in list.
;Assistance Received: none
;********************************************************************* */
(DEFUN printMeld (pile game)
  (cond
    ( (null pile) () )
    (t 
    (cons 
        (printEachMeld (FIRST pile) game)      
        (printMeld (REST pile) game )))))

;/* *********************************************************************
;Function Name: printEachMeld
;Purpose: To print the cards in a meld
;Parameters: 
;     pile - the list of cards
;     game - the list containing all details of the turn
;Return Value: 
;           a list of card values in the meld in console printable form
;Local Variables:
;Algorithm: 
;       1. For each card in meld, check if it is used for multiple melds. If yes add "*" to it.
;       2. If not, just add the card to the return list.
;       3. Return the list containing the card values.
;Assistance Received: none
;********************************************************************* */
(DEFUN printEachMeld (pile game)
	(cond
		( (null pile) () )
		(t 
      (cons (COND 
          ((> (hasMultipleMelds  (FIRST pile) (splitMeld (APPEND (getHumanDeclaredMeld game) (getComputerDeclaredMeld game))) ) 1 )
            (READ-FROM-STRING (concatenate 'string  (STRING (FIRST (FIRST pile))) "*" ))
          )
          (t
            (FIRST (FIRST pile))))      
        (printEachMeld (REST pile) game)))))
  
;/* *********************************************************************
;Function Name: hasMultipleMelds
;Purpose: To check if a card has been used for multiple melds
;Parameters: 
;     card - the card being checked for multiple melds
;     allMelds - all active melds
;Return Value: 
;           the number of active melds linked with the card.
;Local Variables:
;Algorithm: 
;       1. For each meld occurence, add 1 to the return value of the recursive call if the card is found. Else, recursively call function for other melds.
;       2. When allMelds is empty, return 0.
;Assistance Received: none
;********************************************************************* */
(DEFUN hasMultipleMelds (card allMelds)
  (COND
    ((eq (length allMelds) 0)
      0)
    ((AND (eq (FIRST card) (FIRST (FIRST allMelds)) ) (eq (FIRST (REST card)) (FIRST (REST (FIRST allMelds))) ))
      (+ 1 (hasMultipleMelds card (REST allMelds))))
    (t
      (hasMultipleMelds card (REST allMelds)))))

;/* *********************************************************************
;Function Name: splitMeld
;Purpose: To split cards in a meld and create a list of all cards used in meld
;Parameters: 
;     melds - active melds
;Return Value: 
;           a list containing all cards in a meld after splitting card from each meld
;Local Variables:
;Algorithm: 
;       1. For each meld, append all of its card to the return list.
;Assistance Received: none
;********************************************************************* */
(DEFUN splitMeld(melds)
  (COND
    ((eq (length melds) 0)
      ())
    (t
      (APPEND (FIRST melds) (splitMeld (REST melds))))))

;/* *********************************************************************
;Function Name: loadPile
;Purpose: To process a list of cards from a serialization file
;Parameters: 
;     pile - list read from serialization file
;     cards - all cards created in the loading process so far
;Return Value: 
;           a list containing game readable cards
;Local Variables:
;Algorithm: 
;       1. For each card in pile, check if it has already been processed before.
;       2. If not, create a card with index 1 and empty melds.
;       3. If yes, create a card with index 2 and empty melds.
;       4. Append card to the return list.
;Assistance Received: none
;********************************************************************* */
(DEFUN loadPile(pile cards)
  (COND
    ((eq (length pile) 0)
      ())
    (t
      (COND
        ((AND (eq (checkOccurence (FIRST pile) cards) 0))
          (cons (LIST (FIRST pile) 1 ()) (loadPile (REST pile) (cons (FIRST pile) cards ))))
        (t
          (cons (LIST (FIRST pile) 2 ()) (loadPile (REST pile) cards )))))))

;/* *********************************************************************
;Function Name: loadMeldPile
;Purpose: To process meld pile after reading from a serialization file
;Parameters: 
;     pile - list read from serialization file
;     trump - trump card
;     cards - all cards created in the loading process so far
;Return Value: 
;           a list containing game readable cards
;Local Variables:
;Algorithm: 
;       1. For each meld in melds, find the meldType of it and create cards. Attach the meldtype to each card.
;       2. Append cards to the list and return it.
;Assistance Received: none
;********************************************************************* */
(DEFUN loadMeldPile (pile trump cards)
  (COND
    ((eq (length pile) 0)
      ())
    (t
      (APPEND (addMeldToCard (loadPile (FIRST pile) cards) (getMeldType (loadPile (FIRST pile) cards) trump)) (loadMeldPile (REST pile) trump cards)))))

;/* *********************************************************************
;Function Name: loadMelds
;Purpose: To process all active melds
;Parameters: 
;     pile - list read from serialization file
;     trump - trump card
;     cards - all cards created in the loading process so far
;Return Value: 
;           a list containing list of melds.
;Local Variables:
;Algorithm: 
;       1. For each meld in melds, find the meldType of it and create cards. Attach the meldtype to each card.
;       2. Add the list of cards to the return list.
;Assistance Received: none
;********************************************************************* */
(DEFUN loadMelds (pile trump cards)
  (COND
    ((eq (length pile) 0)
      ())
    (t
      (cons   (addMeldToCard (loadPile (FIRST pile) cards) (getMeldType (loadPile (FIRST pile) cards) trump)) (loadMelds (REST pile) trump cards)))))

;/* *********************************************************************
;Function Name: checkOccurence
;Purpose: To check the occurence of a card in a pile
;Parameters: 
;     card - the card whose occurence is being checked
;     cards - the pile of cards
;Return Value: 
;           1 if card is found, 0 if not.
;Local Variables:
;Algorithm: 
;       1. For each card in cards, check if it's equal to supplied card.
;       2. If yes, return 1.
;       3. If not repeat the process for all cards in the pile and if pile gets empty, return 0.
;Assistance Received: none
;********************************************************************* */
(DEFUN checkOccurence(card cards)
  (COND 
    ((eq (length cards) 0)
      0)
    ((eq (FIRST cards) card)
      1)
    (t
      (checkOccurence card (REST cards)))))

    
;/* *********************************************

;Source code to perform necesaary pile changes after both players' turns and setup the game for consecutive turns

;********************************************* */


;/* *********************************************************************
;Function Name: setUpNextTurn
;Purpose: To setup the round for consecutive turns
;Parameters: 
;     game - a list containing all details of the game
;     winner - winner of the turn
;Return Value: 
;          the game list after making all necessary changes to piles and scores
;Local Variables:
;Algorithm: 
;       1. Add played card to winner's capture pile and update scores.
;       2. Allow players to declare meld.
;       2. Hand new cards to players if applicable and make necessary changes to stockpile.
;       3. Return the game list after all changes.
;Assistance Received: none
;********************************************************************* */
(DEFUN setUpNextTurn(game winner)
	(COND
    ;empty stock pile
    ((eq (length (getStockPile game)) 0 )
      (COND
        ((= winner 0)
          (LET*(
            (addToCapturePile (setHumanCapture game (APPEND (getHumanCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
            (updateScore (setHumanRoundScore addToCapturePile (+ (getHumanRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile)))))) 
            (meldDone (declareMeld updateScore winner)))
          meldDone)) 
        (t 
          (LET*(
              (addToCapturePile (setComputerCapture game (APPEND (getComputerCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
              (updateScore (setComputerRoundScore addToCapturePile (+ (getComputerRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile))))))
              (meldDone (declareMeld updateScore winner)))
            meldDone))))

    ;last card in stock pile
    ((eq (length (getStockPile game)) 1 )
      (COND
        ((= winner 0)
          (LET*(
            (addToCapturePile (setHumanCapture game (APPEND (getHumanCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
            (updateScore (setHumanRoundScore addToCapturePile (+ (getHumanRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile)))))) 
            (meldDone (declareMeld updateScore winner))
            (handNewCardToHumanHand (setHumanHand meldDone (APPEND (dealCard 1 (getStockPile meldDone)) (getHumanHand meldDone) )))
            (handNewCardToHumanPlayable (setHumanPlayable handNewCardToHumanHand (APPEND (dealCard 1 (getStockPile handNewCardToHumanHand)) (getHumanPlayable handNewCardToHumanHand) )))
            (handNewCardToComputerHand (setComputerHand handNewCardToHumanPlayable (cons (getTrumpCard game) (getComputerHand handNewCardToHumanPlayable))))
            (handNewCardToComputerPlayable (setComputerPlayable handNewCardToComputerHand (cons (getTrumpCard game) (getComputerPlayable handNewCardToComputerHand))))
            (stockPileChange (setStockPile  handNewCardToComputerPlayable (removeDealtCards 1 (getStockPile handNewCardToComputerPlayable)))))
          stockPileChange))
        (t 
          (LET*(
            (addToCapturePile (setComputerCapture game (APPEND (getComputerCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
            (updateScore (setComputerRoundScore addToCapturePile (+ (getComputerRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile))))))
            (meldDone (declareMeld updateScore winner))
            (handNewCardToComputerHand (setComputerHand meldDone (APPEND (dealCard 1 (getStockPile meldDone)) (getComputerHand meldDone) )))
            (handNewCardToComputerPlayable (setComputerPlayable handNewCardToComputerHand (APPEND (dealCard 1 (getStockPile handNewCardToComputerHand)) (getComputerPlayable handNewCardToComputerHand) )))
            (handNewCardToHumanHand (setHumanHand handNewCardToComputerPlayable (cons (getTrumpCard game) (getHumanHand handNewCardToComputerPlayable))))
            (handNewCardToHumanPlayable (setHumanPlayable handNewCardToHumanHand (cons (getTrumpCard game) (getHumanPlayable handNewCardToHumanHand))))
            (stockPileChange (setStockPile  handNewCardToHumanPlayable (removeDealtCards 1 (getStockPile handNewCardToHumanPlayable)))))
          stockPileChange))))

    ;regular stock pile
    (t
      (COND
      ((= winner 0)
        (LET*(
          (addToCapturePile (setHumanCapture game (APPEND (getHumanCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
          (updateScore (setHumanRoundScore addToCapturePile (+ (getHumanRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile)))))) 
          (meldDone (declareMeld updateScore winner))
          (handNewCardToHumanHand (setHumanHand meldDone (APPEND (dealCard 1 (getStockPile meldDone)) (getHumanHand meldDone) )))
          (handNewCardToHumanPlayable (setHumanPlayable handNewCardToHumanHand (APPEND (dealCard 1 (getStockPile handNewCardToHumanHand)) (getHumanPlayable handNewCardToHumanHand) )))
          (handNewCardToComputerHand (setComputerHand handNewCardToHumanPlayable (APPEND (dealCard 1 (removeDealtCards 1 (getStockPile handNewCardToHumanPlayable))) (getComputerHand handNewCardToHumanPlayable))))
          (handNewCardToComputerPlayable (setComputerPlayable handNewCardToComputerHand (APPEND (dealCard 1 (removeDealtCards 1 (getStockPile handNewCardToComputerHand))) (getComputerPlayable handNewCardToComputerHand))))
          (stockPileChange (setStockPile  handNewCardToComputerPlayable (removeDealtCards 2 (getStockPile handNewCardToComputerPlayable)))))
        stockPileChange))
      (t 
        (LET*(
          (addToCapturePile (setComputerCapture game (APPEND (getComputerCapture game)  (LIST (getHumanPlayed game) (getComputerPlayed game)))) )
          (updateScore (setComputerRoundScore addToCapturePile (+ (getComputerRoundScore addToCapturePile) (+ (getCardPoints (getHumanPlayed addToCapturePile))  (getCardPoints(getComputerPlayed addToCapturePile))))))
          (meldDone (declareMeld updateScore winner))
          (handNewCardToComputerHand (setComputerHand meldDone (APPEND (dealCard 1 (getStockPile meldDone)) (getComputerHand meldDone) )))
          (handNewCardToComputerPlayable (setComputerPlayable handNewCardToComputerHand (APPEND (dealCard 1 (getStockPile handNewCardToComputerHand)) (getComputerPlayable handNewCardToComputerHand) )))
          (handNewCardToHumanHand (setHumanHand handNewCardToComputerPlayable (APPEND (dealCard 1 (removeDealtCards 1 (getStockPile handNewCardToComputerPlayable))) (getHumanHand handNewCardToComputerPlayable))))
          (handNewCardToHumanPlayable (setHumanPlayable handNewCardToHumanHand (APPEND (dealCard 1 (removeDealtCards 1 (getStockPile handNewCardToHumanHand))) (getHumanPlayable handNewCardToHumanHand))))
          (stockPileChange (setStockPile  handNewCardToHumanPlayable (removeDealtCards 2 (getStockPile handNewCardToHumanPlayable)))))
        stockPileChange))))))


;/* *********************************************

;Source code for meld declaration and validation

;********************************************* */

;/* *********************************************************************
;Function Name: declareMeld
;Purpose: To allow users to declare meld after winning a turn.
;Parameters: 
;     game - a list containing all details of the game
;     winner - winner of the turn
;Return Value: 
;          the game list after making all necessary changes to piles and scores
;Local Variables:
;Algorithm: 
;       1. Choose meld cards for the meld.
;       2. Validate the meld and add points if applicable.
;       2. If the meld is not valid, display message indicating so.
;       3. Return the game list after all changes.
;Assistance Received: none
;********************************************************************* */
(DEFUN declareMeld(game winner)
  (COND
    ((eq winner 0)
      (LET*( 
        (meldChoice (getMeldInput )))
      (COND
        ((eq meldChoice 1)
          (TERPRI)
          (PRINC "Please select how many cards [must be 1-5] would you like to choose for the meld : ")
          (LET*(
            (meldCardsNum (inputValidation (READ) 1 5)))
            (COND
              ((>= meldCardsNum 1)
                (LET*(
                  (selectedCards (meldCards (getHumanHand game) (getHumanMeld game) 1 (getHumanHand game) (getHumanMeld game) meldCardsNum))
                  (meld (getMeldType selectedCards (getTrumpCard game)))
                  (legitMeld (isLegitMeld (getTrumpCard game) selectedCards)))
                  (COND 
                    ((AND (eq legitMeld 1) (NOT (eq meld 'None)))
                      (LET*(
                        (scoreUpdated (setHumanRoundScore game (+ (getHumanRoundScore game) (getMeldPoints meld))))
                        (cardsWithMeld (setCardMelds (FIRST selectedCards) (cons meld (FIRST(REST(REST (FIRST selectedCards))))))))
                      (PRINC "You got ")
                      (PRINC (getMeldPoints meld))
                      (PRINC " points for that ")
                      (PRINC meld)
                      (PRINC " meld.")
                      (TERPRI)
                      (updateMeldMap (setMeldCards (setHumanRoundScore game (+ (getHumanRoundScore game) (getMeldPoints meld))) selectedCards meld 0) selectedCards meld 0)))
                    (t 
                      (WRITE-LINE "That's not a valid meld!")
                      (declareMeld game winner)))))
              (t
                nil))))
        ((eq meldChoice 2)
          (LET*(
            (bestMeldCards(getMeldHelp (getHumanPlayable game) (getTrumpCard game))))
          (COND 
            ((eq bestMeldCards NIL)
              (WRITE-LINE "You do not have any valid melds for this turn.")
                (declareMeld game winner))
          (t
            (LET*(
              (meldName (getMeldType bestMeldCards (getTrumpCard game))))
            (PRINC "I recommend you to declare a ")
            (PRINC meldName)
            (PRINC " meld and get ")
            (PRINC (getMeldPoints meldName))
            (PRINC " points with the following cards: ")
            (PRINC (printPile bestMeldCards))
            (TERPRI)
            (declareMeld game winner))))))
        (t 
          game))))
    (t
      (LET*(
        (bestMeldCards(getMeldHelp (getComputerPlayable game) (getTrumpCard game))))
      (COND 
        ((eq bestMeldCards NIL)
          (WRITE-LINE "The computer does not have any valid melds for this turn.")
          game)
        (t
          (LET*(
            (meldName (getMeldType bestMeldCards (getTrumpCard game)))
            (scoreUpdated (setComputerRoundScore game (+ (getComputerRoundScore game) (getMeldPoints meldName))))
            (cardsWithMeld (setCardMelds (FIRST bestMeldCards) (cons meldName (FIRST(REST(REST (FIRST bestMeldCards))))))))
            (PRINC "The computer declared a ")
            (PRINC meldName)
            (PRINC " meld and got ")
            (PRINC (getMeldPoints meldName))
            (PRINC " points with following cards: ")
            (PRINC (printPile bestMeldCards))
            (TERPRI)
            (updateMeldMap (setMeldCards (setComputerRoundScore game (+ (getComputerRoundScore game) (getMeldPoints meldName))) bestMeldCards meldName 1) bestMeldCards meldName 1))))))))

;/* *********************************************************************
;Function Name: getMeldInput
;Purpose: To get input for the meld menu.
;Parameters: 
;Return Value: 
;          the value entered by the user
;Local Variables:
;Algorithm: 
;       Display menu options to the user and get the input after validation
;Assistance Received: none
;********************************************************************* */
(DEFUN getMeldInput ()
      (TERPRI)
      (WRITE-LINE "Meld Declaration Option")
      (WRITE-LINE "----------")
      (WRITE-LINE "1: Declare a Meld.")
      (WRITE-LINE "2: I need help!")
      (WRITE-LINE "3: I do not want to declare a meld.")
      (WRITE-LINE "-----------------")
      (inputValidation (READ) 1 3))

;/* *********************************************************************
;Function Name: meldCards
;Purpose: To get the cards for the meld from human.
;Parameters: 
;         hand - moving hand pile. changes for each recursive call
;         meld - moving meld pile. changes for each recursive call
;         count - variable tracking the number of cards 
;         originalHand - constant handpile. Remains the same for all calls.
;         originalMeld - constant meldpile. Remains the same for all calls.
;Return Value: 
;          the list containing cards supplied by the user
;Local Variables:
;Algorithm: 
;       Display all cards from hand and meld pile and ask user to enter the cards they wish for the meld.
;       Store all cards supplied by the user in a list and return the list.
;Assistance Received: none
;********************************************************************* */
(DEFUN meldCards(hand meld count originalHand originalMeld num)
  (COND 
      ((= count 1)
      (WRITE-LINE "Please select the indexs of the cards that you'd like to choose: ")))
  (COND

    ((= num 0)
      ())

    ((AND (= (length hand) 0) ( = (length meld) 0))
      (COND 
        ((LET*(
            (userChoice (inputValidation (READ) 1 count)))
          (COND
            ((<= userChoice (length originalHand))
              (cons (nth (- userChoice 1) originalHand) (meldCards hand meld count originalHand originalMeld (- num 1))))
            (t
              (cons (nth (- (- userChoice (length originalHand)) 1) originalMeld) (meldCards hand meld count originalHand originalMeld (- num 1)))))))))

    ((> (length hand) 0)
        (COND
          ((= (length hand) (length originalHand))
          (PRINC "Hand: ")))
        (PRINC (FIRST (FIRST hand)))
        (PRINC "(")
        (PRINC count)
        (PRINC ")")
        (PRINC " ")
        (COND
        ((AND (eq (length meld) 0) (eq (length hand) 1))
          (PRINC ": ")))
        (meldCards (REST hand) meld (+ count 1) originalHand originalMeld num))
    (t
      (COND
        ((= (length meld) (length originalMeld))
        (PRINC "Meld: ")))
      (PRINC (FIRST (FIRST meld)))
    (PRINC "(")
    (PRINC count)
    (PRINC ")")
    (PRINC " ")
    (COND
        ((eq (length meld) 1)
          (PRINC ": ")))
    (meldCards hand (REST meld) (+ count 1) originalHand originalMeld num))))

;/* *********************************************************************
;Function Name: getMeldType
;Purpose: To get the meld type from cards supplied.
;Parameters: 
;         cards - cards supplied for the meld
;         trump  - trump card of the eound
;Return Value: 
;          name of the meld
;Local Variables:
;       numCards - number of cards supplied for the meld
;       trumpSuit - the suit of the trump card
;Algorithm: 
;       Depending on the size of supplied cards, check for corresponding meld types.
;       Check requirements for meld and return the meld name if it passes the requirements.
;       If not, return none.
;Assistance Received: none
;********************************************************************* */
(DEFUN getMeldType(cards trump)
  (LET*(
      (numCards (LENGTH cards))
      (trumpSuit (CHAR (STRING (FIRST  trump)) 1)))
    (COND
      ((= numCards 1)
        (COND
          ((AND 
            (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\9 ) 
            (eq (CHAR (STRING (FIRST (FIRST cards))) 1) trumpSuit))
                'Dix)
          (t 'None)))
  
      ((= numCards 2)
        (COND
          ((OR (AND  (AND (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\J ) (eq (CHAR (STRING (FIRST (FIRST cards))) 1) #\D))
                              (AND (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\Q ) (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 1) #\S))
                          )
                (AND  (AND (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\Q ) (eq (CHAR (STRING (FIRST (FIRST cards))) 1) #\S))
                              (AND (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\J ) (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 1) #\D))))
            'Pinochle)

          ((OR (AND  (AND (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\K ) (eq (CHAR (STRING (FIRST (FIRST cards))) 1) trumpSuit))
                              (AND (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\Q ) (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 1) trumpSuit)))
                (AND  (AND (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\Q ) (eq (CHAR (STRING (FIRST (FIRST cards))) 1) trumpSuit))
                              (AND (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\K ) (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 1) trumpSuit))))
            'RoyalMarriage)

          ((AND  (AND (eq (CHAR (STRING (FIRST (FIRST cards))) 1) (CHAR (STRING (FIRST (FIRST (REST cards)))) 1))
                            (OR (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\K ) (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\Q) ))
                      (AND  (OR (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\Q ) (eq (CHAR (STRING (FIRST (FIRST (REST cards)))) 0) #\K))
                        (not (eq (CHAR (STRING (FIRST (FIRST cards))) 0) (CHAR (STRING (FIRST (FIRST (REST cards)))) 0)))))
           'Marriage)
          
           (t 
              'None)))

      ((= numCards 4)
        (LET*(
            (cardFace (CHAR (STRING (FIRST (FIRST cards))) 0))
            (cardSuit (CHAR (STRING (FIRST (FIRST cards))) 1))
            (isSameFace (sameFace cards cardFace (LENGTH cards)))
            (isUniqueSuit (uniqueSuit (REST cards) (- (LENGTH cards) 1) cardSuit 0)))
            (COND
              ((AND(eq isSameFace 1) (eq isUniqueSuit 1))
                (COND
                  ((eq cardFace #\A)
                    'FourAces)
                  ((eq cardFace #\K)
                    'FourKings)
                  ((eq cardFace #\Q)
                    'FourQueens)
                  ((eq cardFace #\J)
                    'FourJacks)
                  (t 
                     'None)))
              (t 'None))))
          
      ((= numCards 5)
        (LET*(
            (cardFace (CHAR (STRING (FIRST (FIRST cards))) 0))
            (isUniqueFace (uniqueFace (REST cards)  (LENGTH cards) cardFace 0))
            (isSameSuit (sameSuit cards trumpSuit (length cards))))
          (COND
            ((AND (eq isUniqueFace 1) (eq isSameSuit 1))
              'Flush)
            (t 'None)))))))

;/* *********************************************************************
;Function Name: getMeldPoints
;Purpose: To get the points of a meld.
;Parameters: 
;       meldType - name of the meld
;Return Value: 
;          the total points a meld is worth
;Local Variables:
;Algorithm: 
;       Check the name of the meld and return the points based on game's rule.
;Assistance Received: none
;********************************************************************* */
(DEFUN getMeldPoints(meldType)
  (COND
    ((eq meldType 'Flush)
      150)
    ((eq meldType 'FourAces)
      100)
    ((eq meldType 'FourKings)
      80)
    ((eq meldType 'FourQueens)
      60)
    ((eq meldType 'FourJacks)
      40)
   ((eq meldType 'RoyalMarriage)
      40)
   ((eq meldType 'Pinochle)
      40)
   ((eq meldType 'Marriage)
      20)
   ((eq meldType 'Dix)
      10)
   (t 0)))

;/* *********************************************************************
;Function Name: sameSuit
;Purpose: To check if two supplied list of cards have the same suit
;Parameters: 
;       cards - a list containing cards
;       suit - the suit which is being checked in all cards
;       length - the number of cards in cards
;Return Value: 
;          1 if all cards have same suit, 0 if not.
;Local Variables:
;Algorithm: 
;       1. Compare the suit of the first card of the list with the suit supplied. If it's not, return 0.
;       2. If it's the same, continue until the end of the list. If suit is same for all, return 1.
;Assistance Received: none
;********************************************************************* */
(DEFUN sameSuit(cards suit length)
  (COND
    ((= length 0)
      1)

  (t
    (COND
      ((eq (CHAR (STRING (FIRST (FIRST cards))) 1) suit) 
        (sameSuit (REST cards) suit (- length 1)))
      (t
        0)))))

;/* *********************************************************************
;Function Name: sameSuit
;Purpose: To check if two supplied list of cards have unique faces
;Parameters: 
;       cards - a list containing cards
;       length - the number of cards in cards
;       face - the face which is being checked in all cards
;       index - variable representing which section of the function to execute
;Return Value: 
;          1 if faces of all cards are unique, 0 if not.
;Local Variables:
;Algorithm: 
;       1. For each card in cards, check if the face appears in the rest of the pile.
;       2. If yes, return 0. If not repeat the process for remaining cards in the pile.
;       3. If the list of cards is empty, return 1 indicating all faces are unique.
;Assistance Received: none
;********************************************************************* */
(DEFUN uniqueFace(cards  length face index)
  (COND
    ((= length 0)
      1)

     ((= index 1)
        (COND 
          ((eq (CHAR (STRING (FIRST (FIRST cards))) 0) face) 
            0)
          (t 
            (uniqueFace (REST cards) (- length 1) face 1))))

    (t
      (COND
        ((OR (eq (CHAR (STRING (FIRST (FIRST cards))) 0) face)  (eq (CHAR (STRING (FIRST (FIRST cards))) 0) #\9) (eq face #\9))
          0)
          
        (t
          (COND

          ((eq (uniqueFace (REST cards ) (- length 1) face 1) 1)
            (uniqueFace (REST cards) (- length 1)  (CHAR (STRING (FIRST (FIRST cards))) 0) 0)
            )
          (t 0)))))))

;/* *********************************************************************
;Function Name: sameFace
;Purpose: To check if two supplied list of cards have the same face
;Parameters: 
;       cards - a list containing cards
;       face - the face which is being checked in all cards
;       length - the number of cards in cards
;Return Value: 
;          1 if all cards have same face, 0 if not.
;Local Variables:
;Algorithm: 
;       1. Compare the face of the first card of the list with the face supplied. If it's not equal, return 0.
;       2. If it's the same, continue until the end of the list. If face is same for all, return 1.
;Assistance Received: none
;********************************************************************* */
(DEFUN sameFace(cards face length)
  (COND
    ((= length 0)
      1)

    (t
      (COND
        ((eq (CHAR (STRING (FIRST (FIRST cards))) 0) face) 
          (sameFace (REST cards) face (- length 1)))
        (t
          0)))))

;/* *********************************************************************
;Function Name: uniqueSuit
;Purpose: To check if two supplied list of cards have unique suits
;Parameters: 
;       cards - a list containing cards
;       length - the number of cards in cards
;       suit - the suit which is being checked in all cards
;       index - variable representing which section of the function to execute
;Return Value: 
;          1 if suit of all cards are unique, 0 if not.
;Local Variables:
;Algorithm: 
;       1. For each card in cards, check if the suit appears in the rest of the pile.
;       2. If yes, return 0. If not repeat the process for remaining cards in the pile.
;       3. If the list of cards is empty, return 1 indicating all suits are unique.
;Assistance Received: none
;********************************************************************* */
(DEFUN uniqueSuit(cards length suit index)
  (COND
    ((= length 0)
      1)

    ((= index 1)
        (COND 
          ((eq (CHAR (STRING (FIRST (FIRST cards))) 1) suit) 
            0)
          (t 
            (uniqueSuit (REST cards) (- length 1) suit 1))))

    (t
      (COND
        ((eq (CHAR (STRING (FIRST (FIRST cards))) 1) suit) 
          0)
          
        (t
          (COND
           ((eq (uniqueSuit (REST cards ) (- length 1) suit 1) 1)
            (uniqueSuit (REST cards) (- length 1)  (CHAR (STRING (FIRST (FIRST cards))) 1) 0))
          (t 
            0)))))))

;/* *********************************************************************
;Function Name: isLegitMeld
;Purpose: To check if a meld is legitimate
;Parameters: 
;       trumpCard - the trump card of the round
;       cards - a list containing cards
;Return Value: 
;          1 if meld is legit, 0 if not.
;Local Variables:
;Algorithm: 
;    1. check if at least one new card has been used for the meld.
;    2. check if any cards have already been used for the same meld previoulsy.
;    3. If new card is used and none of the cards have been reused, return 1 else return 0.
;********************************************************************* */
(DEFUN isLegitMeld (trumpCard cards)
  (LET*(
    (newCard (checkNewMeldCard  (LENGTH cards) cards))
    (reUsedMeldCard (checkReUsedMeldCard (LENGTH cards) cards (getMeldType cards trumpCard))))
    (COND
      ((AND ( = newCard 1) (= reUsedMeldCard 1))
        1)
      (t 
        0))))

;/* *********************************************************************
;Function Name: checkNewMeldCard
;Purpose: To check if at least one card of the meld is a new meld card
;Parameters: 
;       len - length of cards
;       cards - a list containing all cards
;Return Value: 
;          1 if at least one card is a new meld card, 0 if not.
;Local Variables:
;Algorithm: 
;    1. For each card, check the number of melds it has been linked with.
;    2. If the number of melds used for is 0, return 1.
;    3. If not, repeat the process for all remaining cards.
;    4. If length of cards is empty, return 0.
;********************************************************************* */
(DEFUN checkNewMeldCard(len cards)
  (COND
    ((= len 0)
      0)
    ((eq (LENGTH(FIRST(REST(REST (FIRST cards))))) 0)
      1)
    (t (checkNewMeldCard (- len 1) (REST cards) ))))

;/* *********************************************************************
;Function Name: checkReUsedMeldCard
;Purpose: To check if any card has been already used for same meld name
;Parameters: 
;       len - length of cards
;       cards - a list containing all cards
;       name - name of the meld
;Return Value: 
;          1 if none of the cards have been reUsed, 0 if not.
;Local Variables:
;Algorithm: 
;    1. For each card in cards, check if a meld with name exists for the card. 
;    2. If yes, return 0.
;    3. If not, repeat the process for all remaining cards.
;    4. If length of cards is empty, return 1.
;********************************************************************* */
(DEFUN checkReUsedMeldCard(len cards name)
  (COND 
    ((= len 0)
      1)
    ((eq (checkMeldName name (FIRST(REST(REST (FIRST cards))))) 1)
      0)
    (t (checkReUsedMeldCard (- len 1) (REST cards) name))))

;/* *********************************************************************
;Function Name: checkMeldName
;Purpose: To check if any card has been already used for same meld name
;Parameters: 
;       name - name of the meld
;       meldNames - a list containing all meld names a card has been linked with
;Return Value: 
;          1 if name exists in meldName, 0 if not.
;Local Variables:
;Algorithm: 
;    1. for each meldName in meldNames, check if it is equal to name.
;    2. If yes, return 1.
;    3. If not, repeat the process for all remaining cards.
;    4. If length of cards is empty, return 0.
;********************************************************************* */
(DEFUN checkMeldName(name meldNames)
  (COND 
    ((eq (LENGTH meldNames) 0)
      0)
    ((eq name (FIRST meldNames))
      1)
    (t (checkMeldName name (REST meldNames)))))

;/* *********************************************************************
;Function Name: setCardMelds
;Purpose: To add a new meld to a card
;Parameters: 
;       card - card in which the meld is being added
;       meld - name of the meld being added to the card
;Return Value: 
;          the card with updated meld
;Local Variables:
;Algorithm: 
;     add the new meld name to the meld list in a card and return the card.
;********************************************************************* */
(DEFUN setCardMelds (card meld)
  (LIST (FIRST card) (FIRST(REST card)) (cons meld (FIRST(REST(REST card))))))  	

;/* *********************************************************************
;Function Name: updateMeldMap
;Purpose: To update the meld map
;Parameters: 
;       game- list containing all details of the game
;       cards - the new cards to be added to the meldMap
;       meld - name of the meld
;       player - player whose meldMap is updated
;Return Value: 
;          game list with updated meldMap
;Local Variables:
;       currentMeldmap - the current meldMap of the player
;Algorithm: 
;     Add cards with new melds to the current meldMap.
;     Set the declaredMeld to the game list and return it.
;********************************************************************* */
(DEFUN updateMeldMap(game cards meld player)
  (COND
    ((eq player 0)
      (LET*(
        (currentMeldMap (getHumanDeclaredMeld game)))
        (setHumanDeclaredMeld game (cons (addMeldToCard cards meld) currentMeldMap))))
    (t
      (LET*(
        (currentMeldMap (getComputerDeclaredMeld game)))
        (setComputerDeclaredMeld game (cons (addMeldToCard cards meld) currentMeldMap))))))

;/* *********************************************************************
;Function Name: addMeldToCard
;Purpose: To add meld names to a list of cards
;Parameters: 
;       cards - the new cards to be added to the meldMap
;       meld - name of the meld
;Return Value: 
;          a list of cards with the new meld name added
;Local Variables:
;Algorithm: 
;     For each card in cards, add the new meld name to its meld list.
;     Repeat the process for all cards and return the revised list
;********************************************************************* */
(DEFUN addMeldToCard(cards meld)
  (COND
    ((eq (length cards) 0)
      ())
    (t
      (cons (setCardMelds (FIRST cards) meld) (addMeldToCard (REST cards) meld)))))

;/* *********************************************************************
;Function Name: setMeldCards
;Purpose: Update meld cards in all piles and change piles if necessary
;Parameters: 
;       game- list containing all details of the game
;       cards - the new cards to be added to the meldMap
;       meld - name of the meld
;       player - player whose meldMap is updated
;Return Value: 
;          game list after making all changes
;Local Variables:
;Algorithm: 
;     1. For each card in cards, get the card list with new meld added.
;     2. Find the index of the card in playable pile.
;     3. Update the playable pile and replace the card with updated meld list.
;     4. Check if any cards needs to be transferred from Hand to meld and make changes if needed.
;     5. Repeat the process for all card in cards and return game list.
;********************************************************************* */
(DEFUN setMeldCards(game cards meld player)
  (COND
    ((eq (length cards) 0)
      game)
    ((eq player 0)
      (LET*(
        (cardsWithMeld (setCardMelds (FIRST cards) meld )   )
        (cardIndex (getCardIndexInAPile (FIRST cards) (getHumanPlayable game) (getHumanPlayable game)))
        (updatedPlayable (replaceCards (getHumanPlayable game) cardsWithMeld cardIndex ))
        (updatedGame (setHumanPlayable game updatedPlayable ))
        (handToMeldPile (transferFromHandtoMeld updatedGame (FIRST cards) meld player)))
      (setMeldCards handToMeldPile (REST cards) meld player)))
    (t 
      (LET*(
        (cardsWithMeld (setCardMelds (FIRST cards) meld )   )
        (cardIndex (getCardIndexInAPile (FIRST cards) (getComputerPlayable game) (getComputerPlayable game)))
        (updatedPlayable (replaceCards (getComputerPlayable game) cardsWithMeld cardIndex ))
        (updatedGame (setComputerPlayable game updatedPlayable ))
        (handToMeldPile (transferFromHandtoMeld updatedGame (FIRST cards) meld player)))
      (setMeldCards handToMeldPile (REST cards) meld player)))))

;/* *********************************************************************
;Function Name: replaceCards
;Purpose: To replace the cards in a specific indec in a pile with a new card
;Parameters: 
;         pile - the list of cards
;         card- new card which replaces the existing card
;         index - the position of exisiting card in the pile
;Return Value: 
;          the card pile after replacing
;Local Variables:
;Algorithm: 
;       subsitute the new card in the required index and return the pile.
;********************************************************************* */
(DEFUN replaceCards (pile card index)
  (SUBSTITUTE card (ELT pile index) pile :start index :count 1))

;/* *********************************************************************
;Function Name: transferFromHandtoMeld
;Purpose: To transfer cards from hand pile to meld pile
;Parameters: 
;       game- list containing all details of the game
;       card - the card which needs to be added to the meldPile from handPile
;       meld - name of the meld
;       player - player whose meldMap is updated
;Return Value: 
;          the game list after making changes
;Local Variables:
;       handIndex - the index of the card in handpile
;       meldIndex - the index of the card in meldPile
;Algorithm: 
;      Find the index of the card in hand and meld pile.
;      Remove the card from hand pile and add it to meld pile.
;      Make changes to the game list and return it.
;********************************************************************* */
(DEFUN transferFromHandtoMeld(game card meld player)
  (COND
    ((eq player 0)
      (LET*(
        (handIndex (getCardIndexInAPile card (getHumanHand game) (getHumanHand game) ))
        (meldIndex (getCardIndexInAPile card (getHumanMeld game) (getHumanMeld game) )))
      (COND
        ((eq handIndex -1)
          (setHumanMeld game (replaceCards (getHumanMeld game) (setCardMelds card meld )  meldIndex )))
        (t
          (LET*(
            (removeCardFromHand (setHumanHand game (removeCard (+ handIndex 1) (getHumanHand game)))))
          (setHumanMeld removeCardFromHand (cons (setCardMelds card meld)  (getHumanMeld removeCardFromHand) )))))))
    (t 
      (LET*(  
        (handIndex (getCardIndexInAPile card (getComputerHand game) (getComputerHand game) ))
        (meldIndex (getCardIndexInAPile card (getComputerMeld game) (getComputerMeld game) )))
      (COND
        ((eq handIndex -1)
          (setComputerMeld game (replaceCards (getComputerMeld game) (setCardMelds card meld )  meldIndex ) ))
        (t
          (LET*(
            (removeCardFromHand (setComputerHand game (removeCard (+ handIndex 1) (getComputerHand game)))))
          (setComputerMeld removeCardFromHand (cons (setCardMelds card meld)  (getComputerMeld removeCardFromHand) )))))))))

;/* *********************************************

;Source code for computer's meld choices and human recommendations

;********************************************* */

;/* *********************************************************************
;Function Name: getMeldHelp
;Purpose: To get the best meld to declare for computer player and recommended meld for human player
;Parameters: 
;       playablePile - list of all available cards
;       trumpCard - the trump card of the round
;Return Value: 
;          the value returned by the bestMeld function
;Local Variables:
;Algorithm: 
;     1. Generate a list of card combinations of 1,2,4,and 5 cards from the pile.
;     2. Find all possible melds from the pile.
;     3. Find the best possible meld among all possible melds.
;     4. Return the best possible meld.
;********************************************************************* */
(DEFUN getMeldHelp(playablePile trumpCard)
  (LET*(
    (referencedCard (addReferenceCard (length playablePile)))
    (oneCardCombo (generateAllCardsCombo () playablePile 1 0 0 referencedCard 0 ))
    (twoCardCombo (APPEND oneCardCombo (generateAllCardsCombo () playablePile 2 0 0 referencedCard 0 )))
    (fourCardCombo (APPEND twoCardCombo (generateAllCardsCombo () playablePile 4 0 0 referencedCard 0 )))
    (fiveCardCombo (APPEND fourCardCombo (generateAllCardsCombo () playablePile 5 0 0 referencedCard 0 )))
    (possiblemelds (getAllPossibleMelds fiveCardCombo trumpCard)))
    (COND
      ((eq possiblemelds NIL)
          possiblemelds)
        (t
          (bestMeld possiblemelds 0 NIL)))))

;/* *********************************************************************
;Function Name: getAllPossibleMelds
;Purpose: To get all possbible melds from a pile
;Parameters: 
;       combos - a list containing all combinations of 1,2,4 and 5 cards from playable pile.
;       trump - the trump card of the round
;Return Value: 
;          a list conatining all possible meld names and cards which yield the meld.
;Local Variables:
;Algorithm: 
;     1. For each combination in combos. Check if it can yield a legit meld.If yes, add the meld name and cards to the return list.
;     2. Recursively repeat the process for all cards in the meld.
;********************************************************************* */
(DEFUN getAllPossibleMelds (combos trump)
  (COND 
    ((eq (length combos) 0)
      ())
    (t
      (COND
        ((OR (eq (getMeldType (FIRST combos) trump) 'None) (eq (isLegitMeld trump (FIRST combos)) 0) )
          (getAllPossibleMelds (REST combos) trump))
        (t
          (cons (LIST (getMeldType (FIRST combos) trump) (FIRST combos)) 
            (getAllPossibleMelds (REST combos) trump)))
        ))))


;/* *********************************************************************
;Function Name: getAllMeldPoints
;Purpose: To all possible meld points from a list of melds
;Parameters: 
;       A list containg meld names and cards
;Return Value: 
;         The total points combined after adding points of each meld
;Local Variables:
;Algorithm: 
;     1. For each meld in possible melds, compute it's point and add it to the points yielded by the rest of the pile.
;     2. Return the sum of all individual meld points ad total possible meld points.
;********************************************************************* */
(DEFUN getAllMeldPoints (possiblemelds)
  (COND 
    ((eq (length possiblemelds) 0)
      0)
    (t
      (+ (getMeldPoints (FIRST(FIRST possiblemelds)))
        (getAllMeldPoints (REST possiblemelds))))))

;/* *********************************************************************
;Function Name: bestMeld
;Purpose: To find the best meld from a list of melds
;Parameters: 
;       possibleMelds - a list of all melds
;       points - the max points so far
;       cards - the cards yielding the max points so far
;Return Value: 
;         a list of cards which yield the maximum meld points
;Local Variables:
;Algorithm: 
;     1. For each meld, compute the meld point and compare it with max point so far.
;     2. If it's greater than points, update the new point and cards and recursively repeat the process for all cards.
;     3. Return cards when the list is empty.
;********************************************************************* */
(DEFUN bestMeld (possiblemelds points cards)
  (COND
    ((eq (length possiblemelds) 0)
      cards)
    (t
      (LET*(
        (thisMeldPoint (getMeldPoints (FIRST(FIRST possiblemelds)))))
      (COND
        ((> thisMeldPoint points)
          (bestMeld (REST possiblemelds) thisMeldPoint (FIRST (REST (FIRST possiblemelds)))))
        (t
          (bestMeld (REST possiblemelds) points cards)))))))
	
;/* *********************************************

;Source code to print round and game stats to the console

;********************************************* */

;/* *********************************************************************
;Function Name: roundStats
;Purpose: To display the stats of a round to the console
;Parameters: 
;       game - a list containing all game details
;Return Value: 
;Local Variables:
;Algorithm: 
;       Access and print the round scores and total scores for all players.
;********************************************************************* */
(DEFUN roundStats (game)
  (TERPRI)
  (WRITE-LINE "Last Round Scores:")
  (WRITE-LINE "-------------------")
  (PRINC "Human score: ")
  (PRINC (getHumanRoundScore game))
  (TERPRI)
  (PRINC "Computer score: ")
  (PRINC (getComputerRoundScore game))
  (TERPRI)
  (TERPRI)
  (WRITE-LINE "Total Scores:")
  (WRITE-LINE "-------------------")
  (PRINC "Human Total score: ")
  (PRINC (getHumanTotalScore game))
  (TERPRI)
  (PRINC "Computer Total score: ")
  (PRINC (getComputerTotalScore game))
  (TERPRI)
  (WRITE-LINE "-------------------")
  (COND
    ((> (getHumanRoundScore game) (getComputerRoundScore game))
      (WRITE-LINE "Human won the round"))
    ((> (getComputerRoundScore game) (getHumanRoundScore game))
      (WRITE-LINE "Computer won the round"))
    (t
      (WRITE-LINE "Last round was a tie")))
  (TERPRI)
  (TERPRI))

;/* *********************************************************************
;Function Name: getGameWinner
;Purpose: To determine the winner of the game
;Parameters: 
;       game - a list containing all game details
;Return Value: 
;Local Variables:
;Algorithm: 
;       Compare human's and computer round socres and return the index of the winner.
;       If the scores are same, return 2 indicating it's a tie.
;********************************************************************* */
(DEFUN getGameWinner(game)
  (COND
    ((> (getHumanRoundScore game) (getComputerRoundScore game))
      0)
    ((< (getHumanRoundScore game) (getComputerRoundScore game))
      1)
    (t
      2)))

;/* *********************************************************************
;Function Name: gameStats
;Purpose: To display the stats of the game to the console
;Parameters: 
;       game - a list containing all game details
;Return Value: 
;Local Variables:
;Algorithm: 
;       Access and print total socres for both players.
;       Comapre the scores and display the winner.
;       If players have equal points, display message indicating it was a tie.
;********************************************************************* */
(DEFUN gameStats (game)
  (TERPRI)
  (WRITE-LINE "Total Scores:")
  (WRITE-LINE "-------------------------")
  (PRINC "Human Total score: ")
  (PRINC (getHumanTotalScore game))
  (TERPRI)
  (PRINC "Computer Total score: ")
  (PRINC (getComputerTotalScore game))
  (TERPRI)
  (TERPRI)
  (COND
    ((> (getHumanTotalScore game) (getComputerTotalScore game))
      (WRITE-LINE "Human player won the game"))
    ((> (getComputerTotalScore game) (getHumanTotalScore game))
      (WRITE-LINE "Computer player won the game"))
    (t
      (WRITE-LINE "It was a tie")))
  (TERPRI))

;/* *********************************************************************
;Function Name: endGame
;Purpose: To end the game and display a message to the console
;Parameters: 
;Return Value: 
;Local Variables:
;Algorithm: 
;       Display a message and end the game.
;********************************************************************* */
(DEFUN endGame ()
  (TERPRI)
  (WRITE-LINE "------------------------  Thank you for playing Pinochle! ---------------------------")
  (TERPRI))

(Pinochle)