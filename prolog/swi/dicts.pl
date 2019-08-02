:- module(swi_dicts,
          [   % +Key, +Dict0:dict, +OnNotEmpty:callable, +Value, -Dict:dict
              put_dict/5,

              merge_dict/3,             % +Pair, +Dict0:dict, -Dict:dict
              dict_member/2             % ?Dict:dict, ?Member
          ]).

:- use_module(compounds).

:- meta_predicate
    put_dict(+, +, 3, +, -).

%!  put_dict(+Key, +Dict0:dict, +OnNotEmpty:callable, +Value,
%!  -Dict:dict) is det.
%
%   Updates Dict0 to Dict  with  Key-Value,   combining  Value  with any
%   existing value by calling OnNotEmpty/3. The   callable can merge its
%   first two arguments in some  way,  or   replace  the  first with the
%   second, or even reject the second.
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

%!  merge_dict(+Pair:pair, +Dict0:dict, -Dict:dict) is det.
%!  merge_dict(+Value:dict, +Dict0:dict, -Dict:dict) is semidet.
%
%   Merges a key-value Pair, or multiple  pairs from a dictionary Value,
%   into dictionary Dict0, unifying the results   at  Dict. Iterates the
%   pairs for the Value dictionary,  using   them  to recursively update
%   Dict0 key-by-key. Discards the tag from Value; Dict carries the same
%   tag as Dict0.
%
%   Merges non-dictionaries according to type.   Appends  lists when the
%   value in a key-value pair  has   list  type.  Only replaces existing
%   values with incoming values when the leaf   is not a dictionary, and
%   neither existing nor incoming is   a list. Predicate `merge_dict_/3`
%   is the value merging predicate; given   the  original Value0 and the
%   incoming Value, it merges the two values at Value_.

merge_dict(Key-Value, Dict0, Dict) :-
    !,
    put_dict(Key, Dict0, merge_dict_, Value, Dict).
merge_dict(Value, Dict0, Dict) :-
    is_dict(Value),
    dict_pairs(Value, _, Pairs),
    foldl(merge_dict, Pairs, Dict0, Dict).

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

%!  dict_member(?Dict:dict, ?Member) is nondet.
%
%   Unifies Member with all dictionary  members,   where  Member  is any
%   non-dictionary leaf including list elements.
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
%   nested sub-dictionary hierarchies into flatten   lists of tagged-key
%   paths and their leaf values.

dict_member(Dict, Member) :-
    var(Dict),
    !,
    member_dict_(Member, Dict).
dict_member(Dict, Member) :-
    dict_pairs(Dict, Tag, Pairs),
    member(Key-Value0, Pairs),
    dict_member_(Tag^Key-Value0, Member).

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
