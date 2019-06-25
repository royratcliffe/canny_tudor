:- begin_tests(swi_dicts).

:- use_module(dicts).

%   There are no leaf nodes in the dictionary. Asking for a member
%   therefore fails.

test(dict_member, [fail]) :-
    dict_member(settings{advanced:advanced{}}, _).

test(dict_member, [true(A==settings{advanced:advanced{hello:world}}), nondet]) :-
    dict_member(A, settings^advanced/advanced^hello-world).

test(dict_member, [true(A==a{b:c{d:e{f:1}}}), nondet]) :-
    dict_member(A, a^b/c^d/e^f-1).
test(dict_member, [true(A-B==a^b/c^d/e^f/g^h/i^j-999), nondet]) :-
    dict_member(a{b:c{d:e{f:g{h:i{j:999}}}}}, A-B).

:- end_tests(swi_dicts).
