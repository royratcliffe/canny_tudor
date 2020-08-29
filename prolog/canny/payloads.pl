:- module(canny_payloads, [apply_to/2, property_of/2]).

:- meta_predicate apply_to(+, :), property_of(+, :).

:- thread_local payload/2.

visible(Prefix, Suffix, Args, M:Term) :-
    atomic_concat(Prefix, Suffix, Name),
    Term =.. [Name|Args],
    predicate_property(M:Term, visible).

apply_to(Apply, _M:_To) :- var(Apply), !, fail.
apply_to(reset, M:To) :- !, retractall(payload(M:To, _)).
apply_to(push, M:To) :-
    !,
    of(M:To, [new, Payload], Term),
    M:Term,
    asserta(payload(M:To, Payload)).
apply_to(peek(Payload), M:To) :- !, payload(M:To, Payload).
apply_to(pop(Payload), M:To) :- !, retract(payload(M:To, Payload)).
apply_to(Applies, M:To) :-
    is_list(Applies),
    !,
    to(M:To, [_, _, _], Term),
    functor(Term, Name, 3),
    once(apply_to(peek(Payload0), M:To)),
    foldl(M:Name, Applies, Payload0, Payload),
    once(apply_to(pop(_Payload0), M:To)),
    asserta(payload(M:To, Payload)).
apply_to(Apply, M:To) :- to(M:To, [Apply], Term), M:Term.

to(M:To, Args, Term) :- visible(apply_to_, To, Args, M:Term).

property_of(Property, _M:_Of) :- var(Property), !, fail.
property_of(top(Property), M:Of) :-
    !,
    once(apply_to(peek(Payload), M:Of)),
    of(M:Of, [Property, Payload], Term),
    M:Term.
property_of(Property, M:Of) :- of(M:Of, [Property], Term), M:Term.

of(M:Of, Args, Term) :- visible(property_of_, Of, Args, M:Term).
