:- begin_tests(swi_compounds).

:- use_module(compounds).

test(flatten_slashes, [true(A==a/b/c)]) :-
    flatten_slashes(a/(b/c), A).
test(flatten_slashes, [true(A==a/b/c/d), nondet]) :-
    flatten_slashes(a/(b/(c/d)), A).

test(append_path, [true(A==1/2/3/4)]) :-
    append_path(1, 2/3/4, A).
test(append_path, [true(v(A, B, C)==v(a, b, c))]) :-
    append_path(A/B/C, 1/2/3, a/b/c/1/2/3).
test(append_path, [true(A-B==a-b)]) :-
    append_path(A, B, a/b).
test(append_path, [true(A-B==1/2-3)]) :-
    append_path(A, B, 1/2/3).
test(append_path, [true(A==2/3)]) :-
    append_path(1, A, 1/2/3).
test(append_path, [true(A-B==2-3/4/5)]) :-
    append_path(1/A, B, 1/2/3/4/5).
test(append_path, [true(A==1/2/3)]) :-
    append_path(A, 4/5, 1/2/3/4/5).
test(append_path, [true(A-B==1-3/4)]) :-
    append_path(A/2, B/5, 1/2/3/4/5).

:- end_tests(swi_compounds).
