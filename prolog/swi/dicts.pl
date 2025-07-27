:- module(swi_dicts,
          [   % +Key, +Dict0:dict, +OnNotEmpty:callable, +Value, -Dict:dict
              put_dict/5,

              merge_dict/3,             % +Dict0:dict, +Dict1:dict, -Dict:dict
              merge_pair/3,             % +Dict0:dict, +Pair, -Dict:dict
              merge_dicts/2,            % +Dicts:list(dict), -Dict:dict

              dict_member/2,            % ?Dict:dict, ?Member
              dict_leaf/2,              % ?Dict, ?Pair
              dict_pair/2,              % ?Dict, ?Pair

              % ?Tag, ?Template, :Goal, -Dicts:list(dict)
              findall_dict/4,

              dict_tag/2,               % +Dict, ?Tag
              create_dict/3,            % ?Tag, +Dict0, -Dict
              is_key/1,                 % +Key
              dict_compound/2,          % +Dict, -Compound
              list_dict/3               % ?List, ?Tag, ?Dict
          ]).

:- use_module(compounds).
:- use_module(atoms).
:- use_module(lists).

:- meta_predicate
    put_dict(+, +, 3, +, -),
    findall_dict(?, ?, 0, -).

/** <module> SWI-Prolog dictionary extensions
 *
 * This module provides extensions to the SWI-Prolog dictionary
 * implementation. It includes predicates for merging dictionaries,
 * putting values into dictionaries with custom merge behavior,
 * and handling dictionary members and leaves in a more flexible way.
 * It also includes predicates for creating dictionaries from lists
 * and converting dictionaries to compounds.
 *
 * ### Non-deterministic `dict_member(?Dict, ?Member)`
 *
 * This predicate offers an alternative approach to dictionary iteration in
 * Prolog. It makes a dictionary expose its leaves as a list exposes its
 * elements, one by one non-deterministically. It does not unify with
 * non-leaves, as for empty dictionaries.
 *
 * ```prolog
 * ?- dict_member(a{b:c{d:e{f:g{h:i{j:999}}}}}, Key-Value).
 * Key = a^b/c^d/e^f/g^h/i^j,
 * Value = 999.
 *
 * ?- dict_member(Dict, a^b/c^d/e^f/g^h/i^j-999).
 * Dict = a{b:c{d:e{f:g{h:i{j:999}}}}}.
 * ```
 */

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

%!  dict_leaf(-Dict, +Pair) is semidet.
%!  dict_leaf(+Dict, -Pair) is nondet.
%
%   Unifies Dict with its leaf nodes non-deterministically. Each Pair is
%   either  an  atom  for   root-level   keys,    or   a   compound  for
%   nested-dictionary keys. Pair thereby represents   a  nested key path
%   Leaf with its corresponding Value.
%
%   Fails for integer keys because integers   cannot  serve as functors.
%   Does not attempt to map integer  keys   to  an atom, since this will
%   create a reverse conversion disambiguation   issue. This *does* work
%   for nested integer leaf keys, e.g.   a(1), provided that the integer
%   key does not translate to a functor.
%
%   @arg Dict is either a dictionary or  a list of key-value pairs whose
%   syntax conforms to valid dictionary data.

dict_leaf(Dict, Leaf-Value) :- var(Dict), !, leaf_dict_(Dict, Leaf-Value).
dict_leaf(Dict, Leaf-Value) :-
    dict_pairs(Dict, _Tag, Pairs),
    member(Key-Value0, Pairs),
    dict_leaf_(Key-Value0, Leaf-Value).

dict_leaf_(Key-Value0, Leaf-Value) :- is_dict(Value0), !,
    dict_leaf(Value0, Leaf0-Value),
    atom(Key),
    Leaf =.. [Key, Leaf0].
dict_leaf_(Key-Value, Key-Value).

leaf_dict_(Dict, Leaf-Value) :- is_key(Leaf), !,
    dict_create(Dict, _, [Leaf-Value]).
leaf_dict_(Dict, Leaf-Value) :-
    compound(Leaf),
    compound_name_arguments(Leaf, Key, [Leaf_]),
    leaf_dict_(Dict0, Leaf_-Value),
    dict_create(Dict, _, [Key-Dict0]).

%!  dict_pair(+Dict, -Pair) is nondet.
%!  dict_pair(-Dict, +Pair) is det.
%
%   Finds all dictionary  pairs   non-deterministically  and recursively
%   where  each  pair  is  a  Path-Value.   Path  is  a  slash-delimited
%   dictionary key path. Note, the search   fails for dictionary leaves;
%   succeeds  only  for  non-dictionaries.  Fails  therefore  for  empty
%   dictionaries or dictionaries of empty sub-dictionaries.

dict_pair(Dict, Path-Value) :-
    is_dict(Dict),
    !,
    dict_pairs(Dict, _, Pairs),
    member(Key-Value0, Pairs),
    dict_pair_(Key-Value0, Path-Value).
dict_pair(_{}.put(Path, Value), Path-Value).

dict_pair_(Key-Value0, Path-Value) :-
    is_dict(Value0),
    !,
    dict_pair(Value0, Path0-Value),
    append_path(Key, Path0, Path).
dict_pair_(Key-Value, Key-Value).

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

%!  create_dict(?Tag, +Dict0, -Dict) is semidet.
%
%   Creates a dictionary just  like  dict_create/3   does  but  with two
%   important differences. First, the argument  order differs. Tag comes
%   first to make maplist/3 and  convlist/3   more  convenient where the
%   Goal argument includes the Tag. The   new dictionary Dict comes last
%   for the same reason. Secondly, always applies   the given Tag to the
%   new Dict, even if the incoming Data supplies one.
%
%   Creating a dictionary using standard dict_create/3 overrides the tag
%   argument from its Data dictionary,  ignoring   the  Tag  if any. For
%   example, using dict_create/3 for tag xyz  and dictionary abc{} gives
%   you abc{} as the outgoing dictionary.   This predicate reverses this
%   behaviour; the Tag argument replaces any tag in a Data dictionary.

create_dict(Tag, Dict0, Dict) :-
    is_dict(Dict0, _),
    !,
    dict_pairs(Dict0, _, Pairs),
    dict_create(Dict, Tag, Pairs).
create_dict(Tag, Data, Dict) :-
    dict_create(Dict, Tag, Data).

%!  is_key(+Key:any) is semidet.
%
%   Succeeds for terms that  can  serve   as  keys  within a dictionary.
%   Dictionary keys are atoms or  tagged   integers,  otherwise known as
%   constant values. Integers include negatives.
%
%   @arg Key successfully unites for all dictionary-key conforming
%   terms: atomic or integral.

is_key(Key) :- atom(Key), !.
is_key(Key) :- integer(Key).

%!  dict_compound(+Dict:dict, ?Compound:compound) is nondet.
%
%   Finds all compound-folded terms within Dict.  Unifies with all pairs
%   within Dict as compounds of the  form key(Value) where =key= matches
%   the dictionary key converted to one-two style and lower-case.
%
%   Unfolds lists and sub-dictionaries   non-deterministically. For most
%   occasions, the non-deterministic unfolding of   sub-lists results in
%   multiple non-deterministic solutions and  typically   has  a  plural
%   compound name. This is not a perfect  solution for lists of results,
%   since the order of the solutions  defines the relations between list
%   elements.
%
%   Dictionary keys can be  atoms  or   integers.  Converts  integers to
%   compound names using integer-to-atom translation. However, compounds
%   for sub-dictionaries re-wrap the  sub-compounds   by  inserting  the
%   integer key as the prefix argument of a two or more arity compound.

dict_compound(Dict, Compound) :-
    dict_pairs(Dict, _, Pairs),
    member(Key-Value, Pairs),
    dict_compound_(Key-Value, Compound).

dict_compound_(Key-Value, Compound) :-
    is_dict(Value),
    !,
    dict_compound__(Key-Value, Compound).
dict_compound_(Key-Value, Compound) :-
    is_list(Value),
    !,
    member(Member, Value),
    dict_compound_(Key-Member, Compound).
dict_compound_(Key0-Value, Compound) :-
    dict_compound_key(Key0, Key),
    Compound =.. [Key, Value].

dict_compound__(Key-Dict, Compound) :-
    integer(Key),
    !,
    dict_compound(Dict, Compound0),
    Compound0 =.. [Name|Arguments],
    Compound =.. [Name, Key|Arguments].
dict_compound__(Key0-Dict, Compound) :-
    dict_compound(Dict, Compound0),
    dict_compound_key(Key0, Key),
    Compound =.. [Key, Compound0].

dict_compound_key(Key0, Key) :-
    integer(Key0),
    !,
    atom_number(Key, Key0).
dict_compound_key(Key0, Key) :-
    restyle_identifier_ex(one_two, Key0, Key_),
    downcase_atom(Key_, Key).

%!  list_dict(?List, ?Tag, ?Dict) is semidet.
%
%   List to Dict by zipping up items from List with integer indexed keys
%   starting at 1. Finds  only  the   first  solution,  even if multiple
%   solutions exist.

list_dict(List, Tag, Dict) :-
    indexed(List, 1, Pairs),
    dict_create(Dict, Tag, Pairs),
    !.
