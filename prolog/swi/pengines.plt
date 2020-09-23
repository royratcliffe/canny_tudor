:- begin_tests(swi_pengines).

:- use_module(library(pengines)).
:- use_module(pengines).

test(pengine_collect, [true(A-B==A-[1, 2, 3])]) :-
    pengine_create([src_text("a(1). a(2). a(3).")]),
    pengine_collect(A, a(A), B, []).
test(pengine_collect, [true(A-B==A-[1, 2, 3, 4, 5, 6])]) :-
    pengine_create([src_text("a(A) :- between(1, 3, A).")]),
    pengine_create([src_text("a(A) :- between(4, 6, A).")]),
    pengine_collect(A, a(A), B0, []),
    sort(B0, B).

:- end_tests(swi_pengines).
