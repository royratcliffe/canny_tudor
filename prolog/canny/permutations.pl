:- module(canny_permutations,
          [   permute_sum_of_int/2,     % +N, -Integers
              permute_list_to_grid/2    % +List0, -List
          ]).

%!  permute_sum_of_int(+N:nonneg, -Integers:list(integer)) is nondet.
%
%   Permute sum. Non-deterministically finds all combinations of integer
%   sums between 1 and N. Assumes  that   0<=N.  The  number of possible
%   permutations amounts to 2-to-the-power of  N-1;   for  N=3 there are
%   four as follows: 1+1+1+1, 1+2, 2+1 and 3.

permute_sum_of_int(0, []) :- !.
permute_sum_of_int(N, [H|T]) :-
    between(1, N, H),
    N0 is N - H,
    permute_sum_of_int(N0, T).

%!  permute_list_to_grid(+List0:list, -List:list(list)) is nondet.
%
%   Permutes a list to two-dimensional grid, a   list of lists. Given an
%   ordered List0 of elements, unifies List   with  all possible rows of
%   columns. Given =a=, =b= and =c= for  example, permutes three rows of
%   single columns =a=, =b=, =c=; then =a= in the first row with =b= and
%   =c= in the second; then =a= and =b=   in the first row, =c= alone in
%   the second; finally  permutes  =a=,  =b=,   =c=  on  a  single  row.
%   Permutations always preserve the order  of   elements  from first to
%   last.

permute_list_to_grid(List0, List) :-
    length(List0, N),
    permute_sum_of_int(N, Lengths),
    permute_list_to_grid_(List0, Lengths, List).

permute_list_to_grid_([], [], []) :- !.
permute_list_to_grid_(List0, [H0|T0], [H|T]) :-
    length(H, H0),
    append(H, H_, List0),
    permute_list_to_grid_(H_, T0, T).
