:- module(swi_atoms, [restyle_identifier_ex/3]).

%!  restyle_identifier_ex(+Style, +Text, ?Atom) is semidet.
%
%   Restyles Text to Atom.  Predicate   restyle_identifier/3  fails  for
%   incoming     text     with     leading      underscore.     Standard
%   atom:restyle_identifier/3 fails for '_' because underscore fails for
%   atom_codes('_', [Code]), code_type(Code,  prolog_symbol). Underscore
%   (code 95) is a Prolog  variable   start  and identifier continuation
%   symbol, not a Prolog symbol.
%
%   Strips any leading underscore  or   underscores.  Succeeds  only for
%   text, including codes, but does not throw.
%
%   @arg Text string, atom or codes.
%   @arg Atom restyled.

restyle_identifier_ex(Style, Text, Atom) :-
    split_string(Text, "_", "_", Strings),
    atomic_list_concat(Strings, '_', Text_),
    restyle_identifier(Style, Text_, Atom).
