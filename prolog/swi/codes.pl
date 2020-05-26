:- module(swi_codes, [split_lines/2]).

%!  split_lines(?Codes, ?Lines:list(list)) is semidet.
%
%   Splits Codes into Lines of codes,  or   vice  versa.  Lines split by
%   newlines. The last line does not   require  newline termination. The
%   reverse unification however always appends a trailing newline to the
%   last line.

split_lines([], []) :- !.
split_lines(Codes, [Line|More]) :-
    append(Line, [0'\n|Rest], Codes),
    !,
    split_lines(Rest, More).
split_lines(Line, [Line]).
