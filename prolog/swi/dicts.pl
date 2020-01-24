:- module(swi_dicts,
          [   % +Key, +Dict0:dict, +OnNotEmpty:callable, +Value, -Dict:dict
              put_dict/5,

              merge_dict/3,             % +Dict0:dict, +Dict1:dict, -Dict:dict
              merge_pair/3,             % +Dict0:dict, +Pair, -Dict:dict
              merge_dicts/2,            % +Dicts:list(dict), -Dict:dict

              dict_member/2,            % ?Dict:dict, ?Member

              % ?Tag, ?Template, :Goal, -Dicts:list(dict)
              findall_dict/4,

              dict_tag/2                % +Dict, ?Tag
          ]).

:- use_module(compounds).

:- meta_predicate
    put_dict(+, +, 3, +, -),
    findall_dict(?, ?, 0, -).

%!  put_dict(+Key, +Dict0:dict, +OnNotEmpty:callable, +Value,
%!  -Dict:dict) is det.
%
%   Updates dictionary pair calling for  merge   if  not  empty. Updates
%   Dict0 to Dict with Key-Value,  combining   Value  with  any existing
%   value by calling OnNotEmpty/3. The callable  can merge its first two
%   arguments in some way, or replace the first with the second, or even
%   reject the second.
%
%   The implementation puts Key and Value  in Dict0, unifying the result
%   at Dict. However, if the dictionary   Dict0 already contains another
%   value for the indicated Key  then   it  invokes  OnNotEmpty with the
%   original Value0 and  the  replacement   Value,  finally  putting the
%   combined or selected Value_ in the dictionary for the Key.

put_dict(Key, Dict0, OnNotEmpty, Value, Dict) :-
    get_dict(Key, Dict0, Value0),
    !,
    call(OnNotEmpty, Value0, Value, Value_),
    put_dict(Key, Dict0, Value_, Dict).
put_dict(Key, Dict0, _, Value, Dict) :-
    put_dict(Key, Dict0, Value, Dict).

%!  merge_dict(+Dict0:dict, +Dict1:dict, -Dict:dict) is semidet.
%
%   Merges multiple pairs  from  a   dictionary  Dict1,  into dictionary
%   Dict0, unifying the results at  Dict.   Iterates  the  pairs for the
%   Dict1 dictionary, using them to recursively update Dict0 key-by-key.
%   Discards the tag from Dict1; Dict carries the same tag as Dict0.
%
%   Merges non-dictionaries according to type.   Appends  lists when the
%   value in a key-value pair  has   list  type.  Only replaces existing
%   values with incoming values when the leaf   is not a dictionary, and
%   neither existing nor incoming is a list.
%
%   Note the argument order.  The  first   argument  specifies  the base
%   dictionary starting point.  The  second   argument  merges  into the
%   first. The resulting merge unifies at  the third argument. The order
%   only matters if keys collide. Pairs  from Dict1 replace key-matching
%   pairs in Dict0.
%
%   Merging does not replace the original  dictionary tag. This includes
%   an unbound tag. The tag of Dict0 remains unchanged after merge.

merge_dict(Dict0, Dict1, Dict) :-
    is_dict(Dict1),
    dict_pairs(Dict1, _, Pairs),
    foldl(merge_pair_, Pairs, Dict0, Dict).

%!  merge_pair(+Dict0:dict, +Pair:pair, -Dict:dict) is det.
%
%   Merges Pair with dictionary. Merges a key-value Pair into dictionary
%   Dict0, unifying the results at Dict.
%
%   Private predicate `merge_dict_/3`
%   is the value merging predicate; given   the  original Value0 and the
%   incoming Value, it merges the two values at Value_.

merge_pair(Dict0, Key-Value, Dict) :-
    merge_pair_(Key-Value, Dict0, Dict).

merge_pair_(Key-Value, Dict0, Dict) :-
    put_dict(Key, Dict0, merge_dict_, Value, Dict).

%!  merge_dict_(+Value0, +Value, -Value_) is semidet.
%
%   Note, the first argument is the original  one. The second is the new
%   value,  and  the  final  is  an   unbound  variable  waiting  for  a
%   consolidated binding.

merge_dict_(Value0, Value, Dict) :-
    is_dict(Value),
    !,
    merge_dict(Value0, Value, Dict).
merge_dict_(Value0, Value, Values) :-
    is_list(Value),
    !,
    merge_list_(Value0, Value, Values).
merge_dict_(Value0, Value, [Value0|Value]) :-
    is_list(Value0),
    !.
merge_dict_(_, Value, Value).

merge_list_(Value0, Values0, Values) :-
    is_list(Value0),
    !,
    append(Value0, Values0, Values).
merge_list_(Value0, Values0, Values) :-
    append([Value0], Values0, Values).

%!  merge_dicts(+Dicts:list(dict), -Dict:dict) is semidet.
%
%   Merges one or more dictionaries. You cannot merge an empty list of
%   dictionaries. Fails in such cases. It does *not* unify Dict with a
%   tagless empty dictionary. The implementation merges two consecutive
%   dictionaries before tail recursion until eventually one remains.
%
%   Merging ignores tags.

merge_dicts([Dict], Dict) :-
    !.
merge_dicts([Dict0, Dict1|Dicts], Dict) :-
    merge_dict(Dict0, Dict1, Dict_),
    merge_dicts([Dict_|Dicts], Dict).

%!  dict_member(?Dict:dict, ?Member) is nondet.
%
%   Unifies  with  members  of  dictionary.   Unifies  Member  with  all
%   dictionary  members,  where  Member  is   any  non-dictionary  leaf,
%   including list elements, or empty leaf dictionary.
%
%   Keys become tagged keys of the   form  `Tag^Key`. The caret operator
%   neatly fits by operator  precedence   in-between  the  pair operator
%   (`-`) and the sub-key slash  delimiter   (`/`).  Nested  keys become
%   nested    slash-functor    binary    compounds     of    the    form
%   `TaggedKeys/TaggedKey`. So for example, the compound `Tag^Key-Value`
%   translates to Tag{Key:Value}  in   dictionary  form. `Tag^Key-Value`
%   decomposes term-wise as `[-,  Tag^Key,   Value]`.  Note  that tagged
%   keys, including super-sub tagged keys,   take  precedence within the
%   term.
%
%   This is a non-standard approach to  dictionary unification. It turns
%   nested  sub-dictionary  hierarchies  into    flatten  pair-lists  of
%   tagged-key paths and their leaf values.

dict_member(Dict, Member) :-
    var(Dict),
    !,
    member_dict_(Member, Dict).
dict_member(Dict, Member) :-
    dict_pairs(Dict, Tag, Pairs),
    member(Key-Value0, Pairs),
    dict_member_(Tag^Key-Value0, Member).

dict_member_(Tag0^Key0-Tag{}, Tag0^Key0-Tag{}) :-
    !.
dict_member_(Tag0^Key0-Dict, TaggedKeys-Value) :-
    is_dict(Dict),
    !,
    dict_member(Dict, TaggedKeys0-Value),
    flatten_slashes(Tag0^Key0/TaggedKeys0, TaggedKeys).
dict_member_(Member, Member).

%!  member_dict_(+Member, -Dict:dict) is semidet.
%
%   Should value-free members unite? Yes,  allow dictionaries and nested
%   dictionaries using just a Tag without Key  or Value, i.e. just =Tag=
%   rather than `Tag^Key-Value`.

member_dict_(TaggedKeys/Tag^Key-Value, Dict) :-
    !,
    member_dict_(Tag^Key-Value, Dict0),
    member_dict_(TaggedKeys-Dict0, Dict).
member_dict_(Tag^Key-Value, Tag{}.put(Key, Value)) :-
    !.
member_dict_(Tag0^Key/Tag, Tag0{}.put(Key, Dict)) :-
    !,
    member_dict_(Tag, Dict).
member_dict_(Tag, Tag{}) :-
    atom(Tag).

%!  findall_dict(?Tag, ?Template, :Goal, -Dicts:list(dict)) is det.
%
%   Finds all dictionary-only solutions  to   Template  within Goal. Tag
%   selects which tags to select. What happens  when Tag is variable? In
%   such cases, unites with the  first   bound  tag  then all subsequent
%   matching tags.

findall_dict(Tag, Template, Goal, Dicts) :-
    findall(Template, Goal, Bag),
    convlist(findall_dict_(Tag), Bag, Dicts).

:- public
    findall_dict_/3.

findall_dict_(Tag, Dict, Dict) :-
    is_dict(Dict, Tag).

%!  dict_tag(+Dict, ?Tag) is semidet.
%
%   Tags Dict with Tag if currently   untagged.  Fails if already tagged
%   but not matching Tag, just like is_dict/2   with a ground tag. Never
%   mutates ground tags as  a  result.   Additionally  Tags  all  nested
%   sub-dictionaries using Tag and the   sub-key for the sub-dictionary.
%   An underscore delimiter concatenates the tag and key.
%
%   The implementation uses atomic concatenation to   merge  Tag and the
%   dictionary  sub-keys.  Note  that   atomic_list_concat/3  works  for
%   non-atomic keys, including numbers and   strings.  Does not traverse
%   sub-lists. Ignores sub-dictionaries where a   dictionary  value is a
%   list containing dictionaries. Perhaps future versions will.

dict_tag(Dict, Tag) :-
    is_dict(Dict, Tag),
    dict_pairs(Dict, Tag, Pairs),
    pairs_tag(Pairs, Tag).

pairs_tag([], _).
pairs_tag([Key-Value|T], Tag) :-
    (   is_dict(Value)
    ->  atomic_list_concat([Tag, Key], '_', Tag_),
        dict_tag(Value, Tag_)
    ;   true
    ),
    pairs_tag(T, Tag).
