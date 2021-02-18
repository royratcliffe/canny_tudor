:- module(data_frame,
          [ columns_to_rows/2                   % +ListOfColumns,-ListOfRows
          ]).
:- autoload(library(apply), [maplist/4, maplist/3]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(swi/lists)).

%!  columns_to_rows(+ListOfColumns, -ListOfRows) is semidet.
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
%          maplist([B, C]>>dict_create(C, _, B), A, D).
%       A = [[a-1, b-3], [a-2, b-4]],
%       D = [_{a:1, b:3}, _{a:2, b:4}].

columns_to_rows(ListOfColumns, ListOfRows) :-
    maplist([Key=Column, Key, Column]>>true, ListOfColumns, Keys, Columns),
    transpose(Columns, Rows),
    maplist(columns_to_rows_(Keys), Rows, ListOfRows).

columns_to_rows_(Keys, Row0, Row) :-
    zip(Keys, Row0, Row_),
    maplist([[Key, Value], Key-Value]>>true, Row_, Row).
