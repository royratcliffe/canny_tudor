:- module(swi_compounds, [flatten_slashes/2]).

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
