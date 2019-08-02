:- begin_tests(swi_compounds).

:- public test/2.

:- use_module(compounds).

test(flatten_slashes, [true(A==a/b/c)]) :-
    flatten_slashes(a/(b/c), A).
test(flatten_slashes, [true(A==a/b/c/d), nondet]) :-
    flatten_slashes(a/(b/(c/d)), A).

:- end_tests(swi_compounds).
