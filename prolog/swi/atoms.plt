:- begin_tests(swi_atoms).

:- use_module(atoms).

test(restyle_identifier, [throws(error(type_error(atom, []), _))]) :-
    restyle_identifier(one_two, [], _).
test(restyle_identifier, [fail]) :-
    restyle_identifier(one_two, "_", _).
test(restyle_identifier, [fail]) :-
    atom_codes('_', [Code]),
    code_type(Code, prolog_symbol).

test(restyle_identifier_ex, [fail]) :-
    restyle_identifier_ex(one_two, '_', _).
test(restyle_identifier_ex, [fail]) :-
    restyle_identifier_ex(one_two, '', _).
test(restyle_identifier_ex, [true(A=='ABC')]) :-
    restyle_identifier_ex(one_two, [65, 66, 67], A).

test(prefix_atom_suffix, [true(A==abc)]) :-
    prefix_atom_suffix(a, b, c, A).
test(prefix_atom_suffix, [true(A==a)]) :-
    prefix_atom_suffix(A, b, c, abc).
test(prefix_atom_suffix, [all(A-B==[''-ab, a-b, ab-''])]) :-
    prefix_atom_suffix(A, B, c, abc).
test(prefix_atom_suffix, [all(v(A, B, C)==[ v('', '', abc),
                                            v('', a, bc),
                                            v(a, '', bc),
                                            v('', ab, c),
                                            v(a, b, c),
                                            v(ab, '', c),
                                            v('', abc, ''),
                                            v(a, bc, ''),
                                            v(ab, c, ''),
                                            v(abc, '', '')
                                          ])]) :-
    prefix_atom_suffix(A, B, C, abc).

:- end_tests(swi_atoms).
