# Simulating the Probabilistic Aspects of Texas holdem Poker
![](https://img.shields.io/badge/Language_used-The_R_programming_language-blue)

The objective of this project is to explore the chances of winning or tying and improving the player’s 5-card hand at different stages of Texas hold’em poker. The cards dealt are simulated using R software, to find answers to some of the questions based on this game which are somewhat difficult to tackle by applications of pure combinatorics.<br>
In this version of Poker, each hand is played with a whole deck of 52 playing cards, which are divided into 4 suits of 13 cards each i.e. spades ♠, hearts ♥, diamonds ♦, clubs ♣. Cards of spades and clubs are black cards, while that of hearts and diamonds are red cards. The card in each suit, are ace(A), king(K), queen(Q), jack(J), 10, 9, 8, 7, 6, 5, 4, 3 and 2.<br>
One player is the dealer (this role rotates clockwise after each Hand), and bets are placed in a clockwise order starting with the player on the dealer’s left. Each hand has four stages, and after each stage there is a round of betting called blinds. The four stages are:<br>
1.(Pre-flop) Every player gets two starting cards or hole cards (dealt face down)

2.(Flop) 3 community cards are dealt face up in the middle of the table

3.(Turn) 4th community card is dealt face up in the middle of the table

4.(River) 5th community card is dealt face up in the middle of the table.

A Hand is won by having the best Hand among the players who did not fold (i.e. refuse to match an opponent’s bet), or by having everyone else fold. The Hand ends when all but one player has folded or when all the cards have been dealt and the last betting round is over for which, the players must show their cards and the player with the highest Hand wins.

So in this project I have simulated the chances of obtaining each Hand as the best 5-card Hand from a total of 7 cards available for each player, and also their chances of winning and tying with each Hands under varying number of players. Next, I have simulatd the chances of winning with each starting Hand under 2,3 and 4 players. Then I have computed the chances of improving the different types of drawing Hands and made Hands into better ones at the Flop stage, at the Turn stage, at the River stage and also from the Flop to the River stage.
To see the applications of combinatorics involved in this project, you can read my article posted on *[Medium](https://medium.com/@eshan.de22/simulating-the-probabilistic-aspects-in-texas-holdem-poker-ec02857f379f)*.<br>

---

###  This repository includes
- **Simulation Codes**: Contains the simulation codes for the different events focused upon in a game of Texas holdem Poker
- **Diagrams**: Contains all the heat-maps and bar-plots displayed in the project

---

If you have any suggestions or queries, you can reach out to me at my ![LinkedIn](https://img.shields.io/badge/LinkedIn-blue?style=flat&logo=Linkedin) profile <br> 
[Eshan De](https://www.linkedin.com/in/eshan-de-b1635b279/)

Or you can also send me an ![Email](https://img.shields.io/badge/Email-white?style=flat&logo=gmail) at <br>
*Eshan De*: [eshan.de22@gmail.com](mailto:eshan.de22@gmail.com?subject=Regarding%20the%20Poker%20Simulation%20Project%20on%20Github)<br>
