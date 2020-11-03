# Pinochle
Pinochle is a card game played by 2 players.

# The Objective
The objective of this game is to score the most points after all the rounds.

# The Players
Two players play this game - one player is the human user of the program. The other player is the computer.

# The Setup
A modified deck of 48 cards is used.<br />
The deck contains only 9, 10, Jack, Queen, King and Ace - two of each suit.<br />
Each player has a hand and a capture pile.<br />
In addition, a stock pile is part of the game.<br />

# A Round
A round is set up as follows:<br />
- The deck is shuffled.<br />
- The cards are dealt, four at a time, to the two players, starting with the human player, i.e., human player is dealt 4 cards, computer is dealt 4 cards, human player is dealt the next 4 cards and so on till both the players have 12 cards each in their hand.<br />
- The next card is placed face up on the table. The suit of this card is the trump suit.<br />
- The remaining cards are left in the stock pile, face down.<br />

After set up, the first player on the first turn is determined as follows:<br />
- On the first round, a coin is tossed and the human player is asked to call the toss. If the call is correct, the human player plays first. Otherwise, computer plays first.<br />
- On subsequent rounds, the player with the most points from previous rounds plays first. If there is a tie in points, the first player is determined using the mechanism described above for the first round.<br />

The two players each play during each turn until both players have played all the cards in their hands.<br />

# A Turn
During a turn, the two players play as follows:<br />
- The first player plays a card from hand, called the lead card.<br />
- The other player plays a card from hand, called the chase card.<br />
- The winner of the turn is determined as follows:<br />
 &nbsp;- If the lead card is of trump suit, the chase card must be a larger card of trump suit to win.<br />
  Note that cards in increasing value are: 9, Jack, Queen, King, Ten and Ace (highest value).<br />
 &nbsp;- If the lead card is not of trump suit, the chase card must be a larger card of the same suit to win. Alternatively, it can be any card of trump suit to win. If chase card is neither of trump suit, nor of the suit of lead card, lead card wins.<br />
 &nbsp;- If the lead and chase cards are exactly the same card (trump or non-trump), lead card wins.<br />
 
- The winner of the turn adds the lead and chase cards to own capture pile. The winner earns points for the lead and chase cards as follows:<br />
 &nbsp;- 11 points for each Ace<br />
 &nbsp;- 10 points for each Ten card<br />
 &nbsp;- 4 points for each King<br />
 &nbsp;- 3 points for each Queen<br />
 &nbsp;- 2 points for each Jack.<br />
 
- Winner (and only the winner) of the turn can declare a meld using cards from hand. The possible melds and the points earned for each are:<br />
 &nbsp;- Ace, Ten, King, Queen and Jack of Trump suit - called flush - 150 points<br />
 &nbsp;- King and Queen of Trump suit - called royal marriage - 40 points<br />
 &nbsp;- King and Queen of any other suit - called marriage - 20 points<br />
 &nbsp;- Nine of Trump suit - called dix - 10 points<br />
 &nbsp;- Aces of four different suits - called four Aces - 100 points<br />
 &nbsp;- Kings of four different suits - called four Kings - 80 points<br />
 &nbsp;- Queens of four different suits - called four Queens - 60 points<br />
 &nbsp;- Jacks of four different suits - called four Jacks - 40 points<br />
 &nbsp;- Queen of Spades and Jack of Diamonds - called Pinochle - 40 points<br />
 
- The rules for melds are as follows:<br />
 &nbsp;- The player can make only one meld in a turn.<br />
 &nbsp;- The player can add to an earlier meld to create a new meld with a different name, e.g., add King to a Pinochle from an earlier turn to create a marriage. The player cannot add a fifth King to four of Kings to create a new four of Kings meld.<br />
&nbsp;&nbsp; * The meld must contain at least one new card from hand.<br />
 &nbsp;- All the cards in the meld are still part of the hand, and the player must play them on future turns.<br />
The melds of each player are displayed separately from the hand.<br />

- Winner picks the next card from the stock.<br />
- The other player picks the next card from the stock. If the winner picked the last card from the stock, the other player picks the trump card.<br />
 &nbsp;- Note that once the stock is exhausted, the game continues without either player picking any new card.<br />
 
- Winner will play first on the next turn.<br />

# A Game
A game consists of as many rounds as the human player wants to play. The human player is asked before each round whether the player wants to play another round.<br />
If the human player replies yes, another round is started.<br />
If the human player replies no, the game is ended and the player with the most points is declared the winner of the game.<br />

# Score
On each turn, the winner earns points for:<br />
- the two cards captured during the turn;<br />
- any new meld created during that turn.<br />

Winning strategy involves both picking the card from hand to play during each turn and finding a meld to declare during the turn, if available.<br />

# Computer Player's Strategy
The computer player plays to win. It has a strategy for:<br />
- the card to play from its hand<br />
 &nbsp;-  when it is the lead player of the turn - it takes possible melds into account before playng a card;<br />
 &nbsp;-  when it is the chase player of the turn - it tries to win the turn as inexpensively as possible, i.e., use Trump cards only when absolutely necessary.<br />
 
- the meld to declare on each turn it wins - it picks the meld that will yield the most points.<br />
