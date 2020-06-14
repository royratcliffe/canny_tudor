:- module(canny_situations_debugging, [print_situation_history_lengths/0]).

:- use_module(situations).

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

situation([Situation, was(Was, _)]) :-
    !,
    debug(situation, 'situation ~q WAS ~q', [Situation, Was]).
situation([Situation, was(Was, _), now(Now, _)]) :-
    !,
    debug(situation, 'situation ~q WAS ~q NOW ~q', [Situation, Was, Now]).
situation([Situation, now(Now, _)]) :-
    debug(situation, 'situation ~q NOW ~q', [Situation, Now]).

:- initialization listen.

:- use_module(library(print/table)).

%!  print_situation_history_lengths is det.
%
%   Finds all situations.  Samples  their   histories  and  measures the
%   history lengths. Uses `=` when sorting;   do  not remove duplicates.
%   Prints a table of  situations  by   their  history  length,  longest
%   history comes first. Filters out   single-element  histories for the
%   sake of noise minimisation.

print_situation_history_lengths :-
    findall((Module:Situation)-Length,
            (   situation_property(Module:Situation, history(History)),
                length(History, Length)
            ), Situations),
    convlist([First-Second, First-Second]>>(Second > 1), Situations, Filtered),
    sort(2, @>=, Filtered, Sorted),
    print_table(member(_-_, Sorted)).
