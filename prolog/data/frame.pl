:- module(data_frame,
          [ columns_to_rows/2                   % ?ListOfColumns,?ListOfRows
          ]).
:- autoload(library(apply), [maplist/4, maplist/3]).
:- use_module(library(linear/algebra)).

%!  columns_to_rows(?ListOfColumns, ?ListOfRows) is semidet.
%
%   Transforms ListOfColumns to ListOfRows, where a row is a list of
%   key-value pairs, one for each cell. By example,
%
%       [a=[1, 2], b=[3, 4]]
%
%   becomes
%
%       [[a-1, b-3], [a-2, b-4]]
%
%   Else fails if rows or columns do not match. The output list of lists
%   suitably conforms to dict_create/3 Data payloads from which you can
%   build dictionaries.
%
%       ?- columns_to_rows([a=[1, 2], b=[3, 4]], A),
%          maplist([B, C]>>dict_create(C, row, B), A, D).
%       A = [[a-1, b-3], [a-2, b-4]],
%       D = [row{a:1, b:3}, row{a:2, b:4}].

columns_to_rows(ListOfColumns, ListOfRows) :-
    var(ListOfColumns),
    !,
    rows(Keys, Rows, ListOfRows),
    matrix_transpose(Columns, Rows),
    columns(ListOfColumns, Keys, Columns).
columns_to_rows(ListOfColumns, ListOfRows) :-
    columns(ListOfColumns, Keys, Columns),
    matrix_transpose(Columns, Rows),
    rows(Keys, Rows, ListOfRows).

columns(ListOfColumns, Keys, Columns) :-
    maplist([Key=Column, Key, Column]>>true, ListOfColumns, Keys, Columns).

rows(Keys, Rows, ListOfRows) :- maplist(zip(Keys), Rows, ListOfRows).

zip([], [], []).
zip([H1|T1], [H2|T2], [H1-H2|T]) :- zip(T1, T2, T).
