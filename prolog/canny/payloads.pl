:- module(canny_payloads,
          [ payload/1,                  % +M:Payload/{ToArity, OfArity}
            apply_to/1,                 % +M:To/Arity or +M:To/Arities
            apply_to/2,                 % +Apply, +M:To
            property_of/1,              % +M:Of/Arity or +M:Of/Arities
            property_of/2               % +Property, +M:Of
          ]).

:- meta_predicate
    payload(:),
    apply_to(:),
    apply_to(+, :),
    property_of(:),
    property_of(+, :).

:- use_module(arity).

/** <module> Local Payloads
 *
 * Apply and Property  terms  must  be   non-variable.  The  list  below
 * indicates the valid forms of Apply, indicating determinism. Note that
 * only peek and pop perform  non-deterministically for all thread-local
 * payloads.
 *
 *     - `reset` is det
 *     - `push` is semi-det
 *     - peek(Payload) is non-det
 *     - pop(Payload) is non-det
 *     - `[Apply0|Applies]` is semi-det
 *     - `Apply` is semi-det for payload
 *
 * Properties as follows.
 *
 *     - top(Property) is semi-det for payload
 *     - `Property` is semi-det for payload
 *
 * The first form top/1 peeks at  the   latest  payload once. It behaves
 * semi-deterministically for the top-most payload.
 *
 */

:- thread_local payload/2.

payload(M:Payload/{ToArity, OfArity}) :-
    apply_to(M:Payload/ToArity),
    property_of(M:Payload/OfArity).

%!  visible(+Prefix, +Suffix, +Args, :Head) is semidet.
%
%   Finds visible predicates named by  concatenating Prefix with Suffix,
%   with Args specifying the  number  of   arguments  and  also residing
%   within a given module, M. Unifies the result at Head.
%
%   @arg Prefix atom, either `apply_to_` or `property_of_`.
%
%   @arg Suffix must  be  an  instantiated   atom.  You  cannot  pass  a
%   variable. Fails otherwise.

visible(Prefix, Suffix, Args, M:Head) :-
    atom(Suffix),
    atomic_concat(Prefix, Suffix, Name),
    Head =.. [Name|Args],
    predicate_property(M:Head, visible).

apply_to(M:To/Arity) :-
    integer(Arity),
    !,
    atomic_concat(apply_to_, To, Name),
    multifile(M:Name/Arity),
    public(M:Name/Arity).
apply_to(M:To/Arities) :-
    arities(Arities, Arities_),
    forall(member(Arity, Arities_), apply_to(M:To/Arity)).

%!  apply_to(+Apply, :To) is nondet.
%!  apply_to(+Applies, :To) is semidet.
%
%   @arg Applies is a list of  Apply   terms.  It  succeeds when all its
%   Apply terms succeed, and fails when   the  first one fails, possibly
%   leaving side effects if the   apply-to  predicate generates addition
%   effects;  though  typically  not  for    mutation  arity-3  apply-to
%   predicates.

apply_to(Apply, _M:_To) :- var(Apply), !, fail.
apply_to(reset, M:To) :- !, retractall(payload(M:To, _)).
apply_to(push, M:To) :-
    !,
    of(M:To, [new, Payload], Head),
    M:Head,
    asserta(payload(M:To, Payload)).
apply_to(peek(Payload), M:To) :- !, payload(M:To, Payload).
apply_to(pop(Payload), M:To) :- !, retract(payload(M:To, Payload)).
apply_to(Applies, M:To) :-
    is_list(Applies),
    !,
    to(M:To, [_, _, _], Head),
    functor(Head, Name, 3),
    once(apply_to(peek(Payload0), M:To)),
    foldl(M:Name, Applies, Payload0, Payload),
    once(apply_to(pop(_Payload0), M:To)),
    asserta(payload(M:To, Payload)).
apply_to(Apply, M:To) :- to(M:To, [Apply], Head), M:Head.

to(M:To, Args, Head) :- visible(apply_to_, To, Args, M:Head).

property_of(M:Of/Arity) :-
    integer(Arity),
    !,
    atomic_concat(property_of_, Of, Name),
    multifile(M:Name/Arity),
    public(M:Name/Arity).
property_of(M:Of/Arities) :-
    arities(Arities, Arities_),
    forall(member(Arity, Arities_), property_of(M:Of/Arity)).

%!  property_of(+Property, :Of) is nondet.

property_of(Property, _M:_Of) :- var(Property), !, fail.
property_of(top(Property), M:Of) :-
    !,
    once(apply_to(peek(Payload), M:Of)),
    of(M:Of, [Property, Payload], Head),
    M:Head.
property_of(Property, M:Of) :- of(M:Of, [Property], Head), M:Head.

of(M:Of, Args, Head) :- visible(property_of_, Of, Args, M:Head).
