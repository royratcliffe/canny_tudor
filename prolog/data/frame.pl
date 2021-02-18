:- module(data_frame,
          [ columns_to_rows/2                   % +ListOfColumns,-ListOfRows
          ]).
:- autoload(library(apply), [maplist/4, maplist/3]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(swi/lists)).

%!  columns_to_rows(+ListOfColumns, -ListOfRows) is semidet.

columns_to_rows(ListOfColumns, ListOfRows) :-
    maplist([Key=Column, Key, Column]>>true, ListOfColumns, Keys, Columns),
    transpose(Columns, Rows),
    maplist(columns_to_rows_(Keys), Rows, ListOfRows).

columns_to_rows_(Keys, Row0, Row) :-
    zip(Keys, Row0, Row_),
    maplist([[Key, Value], Key-Value]>>true, Row_, Row).
