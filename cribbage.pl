/** <module> Prolog documentation processor

Author:   Kevin Yu
Purpose:  To similate and evaluate hands in the card game Cribbage.
*/

:- use_module(library(clpfd)).

%% card(+Rank, +Suit)
% Definition of a card in the deck
card(Rank, Suit) :-
    member(Rank, 
    [ace, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, 11, queen, 12, king, 13]),
    member(Suit, [clubs, diamonds, hearts, spades]).

%% hand_value(+Hand, +Startcard, -Value)
% Calculate the value of a hand
hand_value(Hand, StartCard, Value) :-
    append(Hand, [StartCard], FullHand),
    sort_hand(FullHand, SortedHand),
    score_hand(SortedHand, Hand, StartCard, Value),
    !. % ! cuts off backtracking
    
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

sort_hand(Hand, SortedHand) :-
    maplist(rank_value, Hand, HandValues),
    msort(HandValues, SortedHandValues),
    maplist(rank_value, SortedHand, SortedHandValues).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Core Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_hand(SortedHand, Hand, Startcard, Value) :-
    score_15s(SortedHand, Points15),
    score_pairs(SortedHand, PointsPairs),
    score_runs(SortedHand, PointsRuns),
    score_flushes(Hand, Startcard, PointsFlushes),
    score_nobs(Hand, Startcard, PointsNobs),
    Value is Points15 + PointsPairs + PointsRuns + PointsFlushes + PointsNobs.

score_15s(SortedHand, Points) :-
    (   bagof(Combination, valid_15s(SortedHand, Combination), Fifteens) ->  
        length(Fifteens, Length)
    ;   Length = 0
    ),
    Points is Length * 2.

%% score_pairs(+Hand, -Points)
% Score pairs in the hand, assuming the hand is sorted
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
% Calculate points for runs in the hand
score_runs(SortedHand, Points) :-
    (   bagof(RunPoints, valid_run(SortedHand, RunPoints), RunPointsList) ->
        findall(RunPoints, valid_run(SortedHand, RunPoints), RunPointsList),
        % We only want the longest run
        max(RunPointsList, Max),
        count_occurrences(RunPointsList, Max, Occurrences),
        Points is Max * Occurrences
    ;   Points = 0
    ).

score_flushes(Hand, StartCard, Points) :-
    (   same_suits([StartCard | Hand]) ->
        Points is 5
    ;   same_suits(Hand) ->
        Points is 4
    ;   Points is 0
    ).

score_nobs(Hand, card(_, Suit), Points) :-
    (   member(card(jack, Suit), Hand) ->
        Points is 1
    ;   Points is 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% valid_15s(+Values, -Combination)
% Generates all valid combinations of cards that sum to 15
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
    combination(Hand, Subset),      % Find all subsets of the hand
    length(Subset, Length),
    Length >= 3,                    % Runs must be at least 3 cards
    maplist(rank_value, Subset, SubsetValues), % Convert cards to values
    is_consecutive(SubsetValues),
    RunPoints = Length.

% Check if the subset is consecutive
is_consecutive([card(Rank1, _), card(Rank2, _)|Rest]) :-
    Rank2 =:= Rank1 + 1,
    is_consecutive([card(Rank2, _)|Rest]).
is_consecutive([_]).  % End of run (single card)

same_suits([_]).
same_suits([card(_, Suit), card(_, Suit)|Rest]) :-
    same_suits([card(_, Suit)|Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generic Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Find the maximum value in a list
max([H | []], H).
max([H | T], Max) :- 
    max(T, Max1), Max1 > H, Max is Max1; 
    Max is H.

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