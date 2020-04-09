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

:- end_tests(swi_atoms).
