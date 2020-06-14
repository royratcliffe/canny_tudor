:- module(swi_compounds, [flatten_slashes/2, append_path/3]).

%!  flatten_slashes(+Components0:compound, ?Components:compound) is
%!  semidet.
%
%   Flattens slash-delimited components. Components0 unifies flatly with
%   Components using mode(+, ?). Fails if   Components  do not match the
%   incoming Components0 correctly with the same number of slashes.
%
%   Consecutive slash-delimited compound terms decompose   in  Prolog as
%   nested slash-functors. Compound =a/b/c= decomposes   to  `/(a/b, c)`
%   for example. Sub-term =a/b= decomposes  to   nested  `/(a,  b)`. The
%   predicate converts any `/(a,  b/c)`  to   `/(a/b,  c)`  so  that the
%   shorthand flattens from `a/(b/c)` to =a/b/c=.
%
%   Note that Prolog variables  match   partially-bound  compounds;  =A=
%   matches `A/(B/C)`. The first argument must therefore be fully ground
%   in order to avoid infinite recursion.
%
%   @tbd Enhance the predicate modes to   allow variable components such
%        as A/B/C; mode (?, ?).

flatten_slashes(Component0/(Component/Components0), Components) :-
    !,
    flatten_slashes(Component0/Component/Components0, Components).
flatten_slashes(Components0/Component, Components/Component) :-
    !,
    flatten_slashes(Components0, Components).
flatten_slashes(Component, Component).

%!  append_path(?Left, ?Right, ?LeftAndRight) is semidet.
%
%   LeftAndRight appends Left path to Right  path. Paths in this context
%   amount to any slash-separated terms,  including atoms and compounds.
%   Paths can include variables. Use  this   predicate  to split or join
%   arbitrary paths. The solutions associate to   the left by preference
%   and collate at Left, even though   the  slash operator associates to
%   the right. Hence append_path(A, B/5,   1/2/3/4/5) gives one solution
%   of A = 1/2/3 and B = 4.
%
%   There is an implementation subtlety. Only find the Right hand key if
%   the argument is really a compound,  not   just  unifies with a slash
%   compound since Path/Component unifies with any unbound variable.

append_path(Left, Right0, LeftAndRight/Component) :-
    compound(Right0),
    !,
    compound_name_arguments(Right0, /, [Right, Component]),
    append_path(Left, Right, LeftAndRight).
append_path(Left, Right, Left/Right) :- !.
append_path(Left, Right/Component, LeftAndRight/Component) :-
    append_path(Left, Right, LeftAndRight).
