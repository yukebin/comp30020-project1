/** <module> Prolog documentation processor

Author:   Kevin Yu
Purpose:  To similate and evaluate hands in the card game Cribbage.
*/

%% rank_value(+Rank, -Value)
%
% Value is the point value of Rank in the game of cribbage. Rank is either an
% integer between 2 and 10, or one of ace, jack, queen, or king. Value is an
% integer between 1 and 13. Your code need only work when Rank is ground.

rank_value(ace, 1).
rank_value(2, 2).
rank_value(3, 3).
rank_value(4, 4).
rank_value(5, 5).
rank_value(6, 6).
rank_value(7, 7).
rank_value(8, 8).
rank_value(9, 9).
rank_value(10, 10).
rank_value(jack, 11).
rank_value(queen, 12).
rank_value(king, 13).

%% hand_value(+Hand, +Startcard, -Value)
%
% Value (an integer) is the total cribbage point value of Hand when Startcard is 
% the start card. Hand is represented as a list of 4 card terms, and Startcard 
% is a single card term. A card term is a term card(Rank, Suit), where Rank is 
% either an integer between 2 and 10, or one of ace, jack, queen, or king, and 
% Suit is one of clubs, diamonds, hearts, or spades. Your code need only work 
% when Hand and Startcard are ground.

%% select_hand(Cards, Hand, Cribcards)
%
% Cards is a list of the 5 or 6 cards dealt to a player at the start of a hand. 
% Hand is a list of 4 of those cards to be kept to form the players hand, and 
% Cribcards is a list of the cards not kept (to be placed in the crib). 
% The cards to be kept in the hand should be chosen to maximize the expected 
% value of the hand over all possible start cards. Since the start card is not 
% known when the hand is chosen, you cannot be sure to choose the best hand. 
% However the expected value of the hand is the average value of the hand over 
% all possible start cards (Cribcards and Hand cards are not possible start 
% cards). This predicate need only work when Cards is a ground list of card 
% terms.