:- begin_tests(swi_dicts).

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
test(dict_member, [true(A==tag{})]) :-
    dict_member(A, tag).
test(dict_member, [true(A==dict^key-value{})]) :-
    dict_member(dict{key:value{}}, A).

test(dict_member, [true(A==settings{advanced:advanced{hello:world}}), nondet]) :-
    dict_member(A, settings^advanced/advanced^hello-world).

test(dict_member, [true(A==a{b:c{d:e{f:1}}}), nondet]) :-
    dict_member(A, a^b/c^d/e^f-1).
test(dict_member, [true(A-B==a^b/c^d/e^f/g^h/i^j-999), nondet]) :-
    dict_member(a{b:c{d:e{f:g{h:i{j:999}}}}}, A-B).

test(dict_leaf, [fail]) :-
    dict_leaf(_{}, _).
test(dict_leaf, [true(A-B=@=_-(a-1))]) :-
    dict_leaf(A{a:1}, B).
test(dict_leaf, [true(v(A, B, C)=@=v(_, _, a(b)-1))]) :-
    dict_leaf(A{a:B{b:1}}, C).
test(dict_leaf, [all(v(A, B, C)=@=[v(_, _, a(b)-1), v(_, _, a(c)-2)])]) :-
    dict_leaf(A{a:B{b:1, c:2}}, C).
test(dict_leaf, [true(A=@=_{a:_{b:_{c:123}}})]) :-
    dict_leaf(A, a(b(c))-123).
test(dict_leaf, [true(A-B=@=_-(1-1))]) :-
    dict_leaf(A{1:1}, B).
test(dict_leaf, [true(A=@=_{1:1})]) :-
    dict_leaf(A, 1-1).
test(dict_leaf, [fail]) :-
    dict_leaf(_{1:_{2:3}}, _).
test(dict_leaf, [true(v(A, B, C)=@=v(_, _, a(2)-3))]) :-
    dict_leaf(A{a:B{2:3}}, C).

test(dict_pair, [fail]) :-
    dict_pair(_{a:_{}}, _).
test(dict_pair, [true(A=@=_{a:_{b:_{c:123}}})]) :-
    dict_pair(A, a/b/c-123).
test(dict_pair, [all(A-B=@=[_-(1-a), _-(2-b), _-(3-c)])]) :-
    dict_pair(A{1:a, 2:b, 3:c}, B).

test(dict_tag, [true(A-B==tag-tag_sub)]) :-
    dict_tag(A{sub:B{}}, tag).

test(create_dict, true(Dict == abc{})) :-
    dict_create(Dict, xyz, abc{}).
test(create_dict, true(Dict == xyz{})) :-
    create_dict(xyz, [], Dict).
test(create_dict, true(A==xyz{})) :-
    create_dict(xyz, abc{}, A).
test(create_dict, true(Dict==Tag{})) :-
    create_dict(Tag, abc{}, Dict).

test(dict_compound, [fail]) :-
    dict_compound(_{}, _).
test(dict_compound, [true(B==a(1)), nondet]) :-
    dict_compound(_{a:1}, B).
test(dict_compound, [true(B==a(1)), nondet]) :-
    dict_compound(_{'A':1}, B).
test(dict_compound, [true(C==a(b(c))), nondet]) :-
    dict_compound(_{a:_{b:c}}, C).
test(dict_compound, [fail]) :-
    dict_compound(_{a:[]}, _).
test(dict_compound, [all(B==[a(1), a(2), a(3)])]) :-
    dict_compound(_{a:[1, 2, 3]}, B).
test(dict_compound, [true(B=='1'(a)), nondet]) :-
    dict_compound(_{1:a}, B).
test(dict_compound, [true(B=='-1'(a)), nondet]) :-
    dict_compound(_{-1:a}, B).
test(dict_compound, [true(C=='999'(123, a)), nondet]) :-
    dict_compound(_{123:_{999:a}}, C).
test(dict_compound, [true(E==a(1, 2, 3, b)), nondet]) :-
    dict_compound(_{1:_{2:_{3:_{a:b}}}}, E).

test(list_dict, [true(A==tag{1:a, 2:b, 3:c})]) :-
    list_dict([a, b, c], tag, A).
test(list_dict, [true(A-B==[a]-tag)]) :-
    list_dict(A, B, tag{1:a}).

:- end_tests(swi_dicts).
