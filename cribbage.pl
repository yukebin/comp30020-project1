/** <module> Prolog documentation processor

Author:   Kevin Yu
Purpose:  To similate and evaluate hands in the card game Cribbage.
*/

:- use_module(library(clpfd)).

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

%% card(Rank, Suit)
%
% A card term is a term card(Rank, Suit), where Rank is either an integer
% between 2 and 10, or one of ace, jack, queen, or king, and Suit is one of
% clubs, diamonds, hearts, or spades.
card(Rank, Suit) :-
    rank_value(Rank, _),
    member(Suit, [clubs, diamonds, hearts, spades]).

%% hand_value(+Hand, +Startcard, -Value)
%
% Value (an integer) is the total cribbage point value of Hand when Startcard is 
% the start card. Hand is represented as a list of 4 card terms, and Startcard 
% is a single card term. A card term is a term card(Rank, Suit), where Rank is 
% either an integer between 2 and 10, or one of ace, jack, queen, or king, and 
% Suit is one of clubs, diamonds, hearts, or spades. Your code need only work 
% when Hand and Startcard are ground.

hand_value(Hand, Startcard, Value) :-
    append(Hand, [Startcard], FullHand),
    convert_hand_to_values(FullHand, Values),
    msort(Values, SortedValues),
    calculate_15s(SortedValues, Points15),
    Value is Points15.

%% sort_cards(+Cards, -Sorted)
sort_cards(Cards, Sorted) :-
    msort(Cards, Sorted).

%% convert_hand_to_values(+Hand, -Values)
convert_hand_to_values([], []).
convert_hand_to_values([card(Rank, _) | Rest], [Value | Values]) :-
    rank_value(Rank, Value),
    convert_hand_to_values(Rest, Values).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% calculate_15s(+SortedValues, -Points)
calculate_15s(SortedValues, Points) :-
    bagof(Combination, valid_combination(SortedValues, Combination), Fifteens),
    length(Fifteens, Length),
    Points is Length * 2.

%% valid_combination(+Values, -Combination)
valid_combination(Values, Combination) :-
    combination(Values, Combination),
    sum_list(Combination, Sum),
    Sum =:= 15.

%% combination(+List, -Combination)
%
% Generates all combinations of elements in List.
combination([], []).
combination([H|T], [H|Comb]) :-
    combination(T, Comb).
combination([_|T], Comb) :-
    combination(T, Comb).

%% sum_list(+List, -Sum)
% 
% A more efficient version of sum_list that stops early if the sum exceeds 15.
sum_list(List, Sum) :-
    sum_list(List, 0, Sum).

sum_list([], Acc, Acc).
sum_list([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    NewAcc =< 15,  % Stop early if the sum exceeds 15
    sum_list(T, NewAcc, Sum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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