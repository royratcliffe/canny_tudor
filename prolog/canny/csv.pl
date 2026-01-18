/*  File:    canny/csv.pl
    Author:  Roy Ratcliffe
    Created: Jan 10 2026
    Purpose: Canny CSV

Copyright (c) 2026, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
permit persons to whom the Software is   furnished  to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

:- module(canny_csv,
          [ csv_read_file_by_row/3              % +Spec, -Row:list, +Options
          ]).
:- autoload(library(apply), [maplist/3, maplist/4]).
:- autoload(library(atom), [restyle_identifier/3]).
:- autoload(library(csv), [csv_read_file_row/3]).
:- autoload(library(option), [option/3]).

/** <module> Canny CSV

This module provides predicates for reading CSV files in a memory-efficient
manner using Prolog engines. The main predicate, `csv_read_file_by_row/3`,
allows for non-deterministic reading of CSV rows, yielding one row at a time.

@author Roy Ratcliffe
*/

%!  csv_read_file_by_row(+Spec, -Row:list, +Options) is nondet.
%
%   Extracts records from a CSV file, using  the given read Options. The
%   resulting Row terms have fields named   after the CSV header columns
%   like an options list.
%
%   This  predicate  uses  a  Prolog  engine    to  read  the  CSV  file
%   non-deterministically, yielding one Row term  at   a  time.  This is
%   useful for processing large CSV  files   without  loading the entire
%   file into memory.
%
%   @arg Spec specifies the CSV file.
%   @arg Row is unified with each row.
%   @arg Options are passed to csv_read_file_row/3.

csv_read_file_by_row(Spec, Row, Options) :-
    absolute_file_name(Spec, Path, [extensions([csv])]),
    engine_create(Row1, csv_read_file_row(Path, Row1, Options), Engine),
    option(functor(Functor), Options, row),

    % Naughty but nice. Read the CSV file for the first time to get the header
    % row, then again to get each data row, mapping the header columns to field
    % names.
    engine_next(Engine, Row0),
    Row0 =.. [Functor|Columns0],
    maplist(restyle_identifier(one_two), Columns0, Columns1),

    % Now read each data row and map to row terms non-deterministically. The row
    % terms have fields named after the header columns. Note that we use
    % engine_next_reified/2 to capture end-of-file and errors. At end-of-file,
    % we cut-fail to stop the iteration. On error, we re-throw the error in the
    % caller's context. Note the difference betwen =/2 and ==/2 here; they are
    % not the same. One tests unification, the other tests identity. Assume that
    % the reified term is either: the(Row), no, or throw(Error). Do not allow
    % for any other possibilities; delegate that responsibility to the engine.
    repeat,
    engine_next_reified(Engine, Term),
    (   Term = the(Row_)
    ->  Row_ =.. [Functor|Columns_],
        maplist(csv_read_file_by_row_, Columns1, Columns_, Row)
    ;   Term == no
    ->  !, fail
    ;   Term = throw(Error)
    ->  throw(Error)
    ).

csv_read_file_by_row_(Name, Value, Row) :- Row =.. [Name, Value].
