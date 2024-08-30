/** This module selects the best hand in Cribbage by maximising the 
    expected value based on possible start cards; select_hand/3.

Author:     Kevin Yu
Student ID: 1462539
Subject:    COMP30020 Declarative Programming

This module contains the implementation of a Cribbage hand selector. It provides 
predicates that selects the best hand, maximising the expected value (points) 
based on every possible start cards. Given the dealt cards, select_hand/3 
returns the best hand that one should keep and the crib cards that should be 
discarded.

This module aims to be used with the full Cribbage game implementation, where 
there will be a dealer and a player. One can use this module as to implement a 
bot, or to provide hints to the player on which cards to keep and discard.

The select_hand/3 predicate works on top of the hand_value/3 predicate, which 
calculates the value of a hand based on the rules. This predicate will also be 
useful for the full Cribbage game where one can use it as a scoring function.
*/

:- use_module(library(clpfd)).

%% card(+Rank, +Suit)
% Definition of a card in the deck, includes 1, 11, 12, 13 for 
% ace, jack, queen, king mapping respectively
card(Rank, Suit) :-
    member(Rank, 
    [ace, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, 11, queen, 12, king, 13]),
    member(Suit, [clubs, diamonds, hearts, spades]).

%% hand_value(+Hand, +Startcard, -Value)
% Calculates the value of a "full hand" with given Hand and Startcard
hand_value(Hand, StartCard, Value) :-
    append(Hand, [StartCard], FullHand),
    sort_hand(FullHand, SortedHand), % Sort the hand for easier scoring
    score_hand(SortedHand, Hand, StartCard, Value),
    !. % ! cuts off backtracking

%% select_hand(+Cards, -Hand, -Cribcards)
% Selects the best Hand with given Cards (5 or 6 cards) and 
% corresponding Cribcards
select_hand(Cards, BestHand, BestCribcards) :-
    % First, generate all possible hands and corresponding crib cards
    bagof(Hand-Cribcards, choose_hand(Cards, Hand, Cribcards), HandCribPairs),
    best_hand(HandCribPairs, BestHand, BestCribcards). % Find the best hand

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Card Value Definition %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% rank_value(+Card, -CardValue)
% Maps rank to numerical value (for sorting and scoring)
rank_value(card(ace, Suit), card(1, Suit)).
rank_value(card(2, Suit), card(2, Suit)).
rank_value(card(3, Suit), card(3, Suit)).
rank_value(card(4, Suit), card(4, Suit)).
rank_value(card(5, Suit), card(5, Suit)).
rank_value(card(6, Suit), card(6, Suit)).
rank_value(card(7, Suit), card(7, Suit)).
rank_value(card(8, Suit), card(8, Suit)).
rank_value(card(9, Suit), card(9, Suit)).
rank_value(card(10, Suit), card(10, Suit)).
rank_value(card(jack, Suit), card(11, Suit)).
rank_value(card(queen, Suit), card(12, Suit)).
rank_value(card(king, Suit), card(13, Suit)).

%% card_value(+Card, -Value)
% Converts card to value for scoring
card_value(card(ace, _), 1).
card_value(card(2, _), 2).
card_value(card(3, _), 3).
card_value(card(4, _), 4).
card_value(card(5, _), 5).
card_value(card(6, _), 6).
card_value(card(7, _), 7).
card_value(card(8, _), 8).
card_value(card(9, _), 9).
card_value(card(10, _), 10).
card_value(card(jack, _), 10).
card_value(card(queen, _), 10).
card_value(card(king, _), 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sort %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% sort_hand(+Hand, -SortedHand)
% Sorts a hand by its rank value
sort_hand(Hand, SortedHand) :-
    maplist(rank_value, Hand, HandValues),
    msort(HandValues, SortedHandValues),
    maplist(rank_value, SortedHand, SortedHandValues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Core Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% score_hand(+SortedHand, +Hand, +Startcard, -Value)
% Calculate the value of a hand
score_hand(SortedHand, Hand, Startcard, Value) :-
    score_15s(SortedHand, Points15),
    score_pairs(SortedHand, PointsPairs),
    score_runs(SortedHand, PointsRuns),
    score_flushes(Hand, Startcard, PointsFlushes),
    score_nobs(Hand, Startcard, PointsNobs),
    Value is Points15 + PointsPairs + PointsRuns + PointsFlushes + PointsNobs.

%% score_15s(+Hand, -Points)
% Calculate Points for 15s in the hand, given Hand
score_15s(Hand, Points) :-
    % Find all the valid combinations of cards that sum to 15
    (   bagof(Combination, valid_15s(Hand, Combination), Fifteens) ->  
        length(Fifteens, Length)
    ;   Length = 0
    ),
    Points is Length * 2.

%% score_pairs(+Hand, -Points)
% Calculate Points for pairs in the Hand, assuming the Hand is sorted
score_pairs([], 0).
score_pairs([card(Rank, _), card(Rank, _)|Rest], Points) :-
    % Check for three-of-a-kind
    (Rest = [card(Rank, _)|Rest2] ->
        % Check for four-of-a-kind
        (Rest2 = [card(Rank, _)|Rest3] ->
            score_pairs(Rest3, SubPoints),
            Points is SubPoints + 12
        ;   score_pairs(Rest2, SubPoints),
            Points is SubPoints + 6
        )
    ;   score_pairs(Rest, SubPoints),
        Points is SubPoints + 2
    ).
score_pairs([_|Rest], Points) :-
    score_pairs(Rest, Points).

%% score_runs(+Hand, -Points)
% Calculate Points for runs in the Hand, assuming the Hand is sorted
score_runs(SortedHand, Points) :-
    (   bagof(RunPoints, valid_run(SortedHand, RunPoints), RunPointsList) ->
        % We only want the longest run
        max(RunPointsList, Max),
        count_occurrences(RunPointsList, Max, Occurrences),
        Points is Max * Occurrences
    ;   Points = 0
    ).

%% score_flushes(+Hand, +StartCard, -Points)
% Calculate Points for flushes in the hand
score_flushes(Hand, StartCard, Points) :-
    % 4 points if all hand cards are the same suit; 
    % 1 more if the start card matches.
    (   same_suits([StartCard | Hand]) ->
        Points is 5
    ;   same_suits(Hand) ->
        Points is 4
    ;   Points is 0
    ).

%% score_nobs(+Hand, +StartCard, -Points)
% Calculate Points for nobs in the hand
score_nobs(Hand, card(_, Suit), Points) :-
    % a point if the hand contains a jack of the same suit as the start card
    (   member(card(jack, Suit), Hand) ->
        Points is 1
    ;   Points is 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Hand Selection Core %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% choose_hand(+Cards, -Hand, -Cribcards)
% Generate all possible Hands (4 cards) and corresponding Cribcards
choose_hand(Cards, Hand, Cribcards) :-
    length(Hand, 4),
    combination(Cards, Hand),
    subtract(Cards, Hand, Cribcards).

%% best_hand(+HandCribPairs, -BestHand, -BestCribcards)
% Find the best Hand based on expected value of all possible start cards
best_hand([Hand-Cribcards | Rest], BestHand, BestCribcards) :-
    expected_value(Hand, Value),
    best_hand(Rest, Hand-Cribcards, Value, BestHand, BestCribcards).

% best_hand/5 helper function with accumulator
best_hand([], BestHand-Cribcards, _, BestHand, Cribcards).
best_hand([Hand-Cribcards | Rest], CurrentBestHand-CurrentBestCribcards, 
            CurrentBestValue, BestHand, BestCribcards) :-
    expected_value(Hand, Value),
    % Update the best hand if the current hand has a higher expected value
    (   Value > CurrentBestValue ->
        best_hand(Rest, Hand-Cribcards, Value, BestHand, BestCribcards)
    ;   best_hand(Rest, CurrentBestHand-CurrentBestCribcards, CurrentBestValue, 
                    BestHand, BestCribcards)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% valid_15s(+Values, -Combination)
% Generates all valid Combinations of cards that sum to 15
valid_15s(Hand, Combination) :-
    combination(Hand, Combination),
    sum_to_15(Combination).

%% sum_to_15(+Combination)
% Check if the sum of the ranks of cards equals 15
sum_to_15(Combination) :-
    maplist(card_value, Combination, Values),
    sum_list(Values, Sum),
    Sum = 15.

%% valid_run(+Hand, -RunPoints)
% Generate all valid runs and return their point value
valid_run(Hand, RunPoints) :-
    combination(Hand, Subset),                 % Find all subsets of the hand
    length(Subset, Length),
    Length >= 3,                               % Runs must be at least 3 cards
    maplist(rank_value, Subset, SubsetValues), % Convert cards to values
    is_consecutive(SubsetValues),
    RunPoints = Length.

%% is_consecutive(+Cards)
% Checks if the cards are consecutive
is_consecutive([card(Rank1, _), card(Rank2, _)|Rest]) :-
    Rank2 =:= Rank1 + 1,
    is_consecutive([card(Rank2, _)|Rest]).
is_consecutive([_]).  % End of run (single card)

%% same_suits(+Hand)
% Check if all cards have the same suit
same_suits([_]).
same_suits([card(_, Suit), card(_, Suit)|Rest]) :-
    same_suits([card(_, Suit)|Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Hand Selection Helper %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% expected_value(+Hand, -Value)
% Calculate the expected value of a hand
expected_value(Hand, Value) :-
    % First, generate all possible start cards
    bagof(StartCard, possible_start_card(StartCard, Hand), PossibleStartCards),
    % Calculate the value of each possible start card and original Hand
    maplist(hand_value(Hand), PossibleStartCards, Values),
    sum_list(Values, Total),
    length(PossibleStartCards, Count),
    Value is Total / Count. % Expected value is the average of possible values

%% possible_start_card(-Card, +Hand)
% Check if a card is a valid start card
possible_start_card(card(Rank, Suit), Hand) :-
    card(Rank, Suit),                
    \+ member(card(Rank, Suit), Hand),
    \+ member(Rank, [1, 11, 12, 13]).   % Face cards cannot be number cards

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generic Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% max(+List, -Max)
% Find the maximum value in a list
max([H | []], H).
max([H | T], Max) :- 
    max(T, Max1), Max1 > H, Max is Max1; 
    Max is H.

%% count_occurrences(+List, +Value, -Count)
% Count occurrences of a value in a list
count_occurrences([], _, 0).
count_occurrences([H|T], Value, Count) :-
    (   H =:= Value ->
        count_occurrences(T, Value, TailCount),
        Count is TailCount + 1
    ;   count_occurrences(T, Value, Count)
    ).

%% combination(+List, -Combination)
% Generates all combinations of elements in List.
combination([], []).
combination([H|T], [H|Comb]) :-
    combination(T, Comb).
combination([_|T], Comb) :-
    combination(T, Comb).