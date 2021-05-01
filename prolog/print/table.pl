:- module(print_table,
          [ print_table/1,                      % :Goal
            print_table/2                       % :Goal,+Variables:list
          ]).

:- meta_predicate print_table(0).
:- meta_predicate print_table(0, +).

:- use_module(library(clpfd), [transpose/2]).
:- use_module(library(swi/lists)).

%!  print_table(:Goal) is det.
%!  print_table(:Goal, +Variables:list) is det.
%
%   Prints all the variables  within   the  given non-deterministic Goal
%   term  formatted  as   a   table    of   centre-padded   columns   to
%   =current_output=. One Goal  solution  becomes   one  line  of  text.
%   Solutions to free variables become printed cells.
%
%   Makes an important  assumption:  that   codes  equate  to  character
%   columns; one code, one column. This will  be true for most languages
%   on a teletype like terminal. Ignores any exceptions by design.
%
%       ?- print_table(user:prolog_file_type(_, _)).
%       +------+----------+
%       |  pl  |  prolog  |
%       |prolog|  prolog  |
%       | qlf  |  prolog  |
%       | qlf  |   qlf    |
%       | dll  |executable|
%       +------+----------+

print_table(Goal) :-
    term_variables(Goal, Variables),
    print_table(Goal, Variables).

print_table(Goal, Variables) :-
    findall(Variables, Goal, Rows0),
    maplist(
        maplist(
            [Column, Codes]>>
            with_output_to_codes(
                print(Column), Codes)), Rows0, Rows),
    transpose(Rows, Columns),
    maplist(maplist(length), Columns, Lengths),
    maplist(max_list, Lengths, Widths),
    print_border(Widths),
    forall(member(Row, Rows), print_row(Widths, Row)),
    print_border(Widths).

print_row(Widths, Row) :-
    zip(Widths, Row, Columns),
    forall(member(Column, Columns), print_column(Column)),
    put_code(0'|),
    nl.

print_column([Width, Column]) :-
    format('|~|~t~s~t~*+', [Column, Width]).

print_border(Widths) :-
    forall(member(Width, Widths), format('+~|~`-t~*+', [Width])),
    put_code(0'+),
    nl.
