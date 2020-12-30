:- begin_tests(swi_lists).

:- use_module(lists).

test(zip) :-
    zip([], [], []),
    zip([1], [2], [[1, 2]]),
    zip([1, a], [2, b], [[1, 2], [a, b]]).

test(zip, [true(A-B==[1, 3]-[2, 4])]) :-
    zip(A, B, [[1, 2], [3, 4]]).

test(pairs, [true(A==[1-2])]) :-
    pairs([1, 2], A).
test(pairs, [fail]) :-
    pairs([1, 2, 3], _).
test(pairs, [true(A==[1, 2])]) :-
    pairs(A, [1-2]).
test(pairs, [true(A==[1, 2, 3, 4])]) :-
    pairs(A, [1-2, 3-4]).

test(indexed_pairs) :-
    indexed_pairs([], 1, []).
test(indexed_pairs, [true(A-B==[1]-1)]) :-
    indexed_pairs(A, B, [1-1]).
test(indexed_pairs, [true(A-B==[1, 2, 3]-1)]) :-
    indexed_pairs(A, B, [1-1, 2-2, 3-3]).
test(indexed_pairs, [true(A==[0-first, 1-second, 2-third])]) :-
    indexed_pairs([first, second, third], 0, A).
test(indexed_pairs, [true(A==1)]) :-
    indexed_pairs([first], A, [1-first]).

test(take_at_most, [true(A==[])]) :- take_at_most(0, [1, 2, 3], A).
test(take_at_most, [true(A==[1])]) :- take_at_most(1, [1, 2, 3], A).
test(take_at_most, [true(A==[1, 2])]) :- take_at_most(2, [1, 2, 3], A).
test(take_at_most, [true(A==[1, 2, 3])]) :- take_at_most(3, [1, 2, 3], A).
test(take_at_most, [true(A==[1, 2, 3])]) :- take_at_most(4, [1, 2, 3], A).
test(take_at_most, [true(A==[1, 2])]) :- take_at_most(5, [1, 2], A).

test(select1, [true(A==[c, b, a])]) :-
    select1([3, 2, 1], [a, b, c], A).
test(select1, [true(A==[a])]) :-
    select1([1], [a, b, c], A).

:- end_tests(swi_lists).
