:- begin_tests(in_temporary).

:- use_module(temporary).

test(ask_in_temporary, [true(A=@=[ask(_)])]) :-
    ask_in_temporary(':- public ask/1. ask(_).', A, []).
test(ask_in_temporary) :-
    ask_in_temporary("", _, []).
test(ask_in_temporary, [true(A==[ask(1), ask(2), ask(3)])]) :-
    ask_in_temporary(":- public ask/1. ask(A) :- between(1, 3, A).", A, []).

:- end_tests(in_temporary).
