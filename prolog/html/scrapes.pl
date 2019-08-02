:- module(html_scrapes, [scrape_row/2]).

:- use_module(library(sgml)).

%!  scrape_row(+URL, -Row) is nondet.
%
%   Scrapes all table rows  non-deterministically   by  row  within each
%   table. Tables must have table headers, =thead= elements.
%
%   Scrapes distinct rows. Distinct is  important because HTML documents
%   contain tables within tables within tables.  Attempts to permit some
%   flexibility. Asking for sub-rows finds   head  sub-rows; catches and
%   filters out by disunifying data with heads.

scrape_row(URL, Row) :-
    distinct(Row, scrape_row_(URL, Row)).

scrape_row_(URL, Row) :-
    load_html(URL, DOM, []),
    xpath(DOM, //(table), Table),
    findall(Head, xpath(Table, //(thead)/tr/td(normalize_space), Head), Heads),
    xpath(Table, //(tr), TR),
    findall(Datum, xpath(TR, //(td(normalize_space)), Datum), Data),
    Data \= Heads,
    scrape_row__(Heads, Data, Columns),
    Row =.. [row|Columns].

scrape_row__([], [], []).
scrape_row__([Head0|Heads], [Datum|Data], [Column|Columns]) :-
    restyle_identifier(one_two, Head0, Head),
    Column =.. [Head, Datum],
    scrape_row__(Heads, Data, Columns).
