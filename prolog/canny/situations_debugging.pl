:- module(canny_situations_debugging, [print_situation_history_lengths/0]).

listen :-
    unlisten,
    debug(situation),
    listen(situation(A), situation([A])),
    listen(situation(A, B), situation([A, B])),
    listen(situation(A, B, C), situation([A, B, C])).

unlisten :-
    context_module(M),
    unlisten(M),
    nodebug(situation).

situation([_, fix]) :- !.
situation(Arguments) :-
    Term =.. [situation|Arguments],
    debug(situation, '~q', [Term]).

:- initialization listen.

:- use_module(library(print/table)).

%!  print_situation_history_lengths is det.

print_situation_history_lengths :-
    findall((Module:Situation)-Length,
            (   situation_property(Module:Situation, history(History)),
                length(History, Length)
            ), Situations),
    convlist([First-Second, First-Second]>>(Second > 1), Situations, Filtered),
    sort(2, @>=, Filtered, Sorted),
    print_table(member(_-_, Sorted)).
