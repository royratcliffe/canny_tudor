:- begin_tests(swi_options).

:- use_module(options).

% Tests for select_options/4 predicate. The predicate should select options
% based on the provided terms and options. The first argument is a list of
% terms, the second is a list of options to select from, the third is a list of
% options to exclude, and the fourth is the resulting list of selected options.
test(select_options, [true(var(A))]) :-
    select_options([term(A)], [], [], []).
test(select_options, [true(A==x)]) :-
    select_options([term(A)], [], [], [term(x)]).
test(select_options, A-B-Rest == 1-2-[c(3)]) :-
    select_options([a(A), b(B)], [a(1), b(2), c(3)], Rest, [a(0), b(0)]).

:- end_tests(swi_options).
