:- begin_tests(swi_dicts).

:- public test/2.

:- use_module(dicts).

test(merge_dict, [true(v(Tag, Dict)==v(Tag, Tag{a:2}))]) :-
    merge_dict(Tag{a:1}, Tag{a:2}, Dict).
test(merge_dict, [true(A==x{a:2})]) :-
    merge_dict(x{a:1}, y{a:2}, A).
test(merge_dict, [true(Tag-Dict==Tag-Tag{a:2})]) :-
    merge_dict(Tag{a:1}, y{a:2}, Dict).
test(merge_dict, [true(Tag-Dict==Tag-Tag{a:2})]) :-
    merge_dicts([Tag{a:1}, y{a:2}], Dict).

test(dict_member, [true(A==settings^advanced-advanced{})]) :-
    dict_member(settings{advanced:advanced{}}, A).

test(dict_member, [true(A==dict{key:value{}})]) :-
    dict_member(A, dict^key-value{}).
test(dict_member, [true(A==dict^key-value{})]) :-
    dict_member(dict{key:value{}}, A).

test(dict_member, [true(A==settings{advanced:advanced{hello:world}}), nondet]) :-
    dict_member(A, settings^advanced/advanced^hello-world).

test(dict_member, [true(A==a{b:c{d:e{f:1}}}), nondet]) :-
    dict_member(A, a^b/c^d/e^f-1).
test(dict_member, [true(A-B==a^b/c^d/e^f/g^h/i^j-999), nondet]) :-
    dict_member(a{b:c{d:e{f:g{h:i{j:999}}}}}, A-B).

test(dict_tag, [true(A-B==tag-tag_sub)]) :-
    dict_tag(A{sub:B{}}, tag).

:- end_tests(swi_dicts).
