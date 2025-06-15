/*  File:    scasp_just_dot.pl
    Author:  Roy Ratcliffe
    Created: Jun  8 2025
    Purpose:
*/

:- module(scasp_just_dot,
          [ scasp_print_just_dot/3              % Stream, Src, Options
          ]).
:- autoload(library(http/json), [json_read_dict/2]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(option), [select_option/4, option/3]).
:- use_module(library(settings), [setting/4, setting/2]).
:- autoload(library(dcg/high_order), [sequence/4]).

%!  scasp_print_just_dot(+Stream, +Src, +Options) is det.

scasp_print_just_dot(Stream, Src, Options) :-
    read_json_dict(Src, Dict),
    phrase(json_dot(Dict, Options), Lines),
    print_message_lines(Stream, '', Lines).

read_json_dict(Src, Dict) :-
    setup_call_cleanup(open(Src, read, Stream),
                       json_read_dict(Stream, Dict),
                       close(Stream)).

:- setting(tab, nonneg, 2, '').
:- setting(rankdir, atom, 'LR', '').
:- setting(bgcolor, atom, transparent, '').
:- setting(node, list(compound), [ style=filled,
                                   fillcolor=lightyellow,
                                   color=darkred,
                                   fontname="Arial",
                                   fontsize=10
                                 ], '').
:- setting(edge, list, [color=darkred], '').

json_dot(Tree, Options) -->
    line_dot('digraph {', Options, Options_),
    answers_dot(Tree.answers, Options_),
    line_dot('}', Options).

answers_dot(Answers, Options) -->
    setting_dot(rankdir, Options),
    setting_dot(bgcolor, Options),
    setting_dot(node, Options),
    setting_dot(edge, Options),
    sequence(answer_dot(Options), Answers).

answer_dot(Options, Answer) -->
    line_dot('// answer ~d'-[Answer.answer], Options),
    line_dot('// time ~f'-[Answer.time], Options),
    answer_tree_dot(Answer.tree, Options).

answer_tree_dot(_{node:Node, children:Children}, Options) -->
    {value_term(Node.value, query)},
    sequence(query_dot(Options), Children).

query_dot(Options, Answer) -->
    line_dot('subgraph {', Options),
    line_dot('}', Options).

implies_dot(Node0, Options, Node) -->
    line_dot('"~w" -> "~w";'-[Node0, Node], Options).

dict_comment_dot(Dict, Options) -->
    {dict_pairs(Dict, _, Pairs)},
    sequence(pair_comment_dot(Options), Pairs).

pair_comment_dot(Options, Key-Value) -->
    {includes_or_not_excludes(Key, Options)},
    !,
    line_dot('// ~k ~k'-[Key, Value], Options).
pair_comment_dot(_, _) --> [].

includes_or_not_excludes(Key, Options) :-
    (   option(includes(Includes), Options)
    ->  memberchk(Key, Includes)
    ;   option(excludes(Excludes), Options)
    ->  \+ memberchk(Key, Excludes)
    ).

setting_dot(Name, Options) -->
    {setting(Name, Value)},
    setting_dot(Name, Value, Options).

setting_dot(Name, Value, Options) --> {is_list(Value)}, !,
    line_dot('~p ~p;'-[Name, Value], Options).
setting_dot(Name, Value, Options) -->
    line_dot('~w;'-[Name=Value], Options).

tab_dot(Options0, Options) --> {indent(Options0, Tab, Options)}, [Tab].

tab_dot(Options) --> tab_dot(Options, _).

line_dot(Line, Options0, Options) --> tab_dot(Options0, Options), [Line, nl].

line_dot(Line, Options) --> line_dot(Line, Options, _).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

value_p(Value) --> {value_term(Value, Term)}, ['~p'-[Term]].

value_w(Value) --> {value_term(Value, Term)}, ['~w'-[Term]].

%!  value_term(+Value:dict, -Term) is semidet.
%
%   Converts a Prolog dictionary (parsed from JSON) that represents a term
%   into an actual Prolog term (either an atom or a compound). The predicate
%   deals with two cases:
%
%       1. When the dictionary represents an atom (`type:"atom"`), it
%       converts the string value to an atom.
%
%       2. When the dictionary represents a compound
%       (`type:"compound"`), it recursively processes the arguments and
%       constructs the compound term.
%
%   It employs dictionary pattern matching (`:<`) for clarity and safety and
%   handles nested compounds by recursively mapping arguments. The predicate
%   is "semidet," i.e. succeeds at most once; but if the input dictionary
%   does not match either pattern, it fails silently. If the JSON structure
%   changes or new term types are added, the predicate will require
%   updating.
%
%   Terms here refer to either a compound or an atom. Compounds consist of
%   atoms, pertaining to the functor and the compound's arguments. Node
%   values assume the form of a dictionary after loading the JSON. The JSON
%   transforms into a Prolog dictionary composed of dictionaries and lists.
%   The implementation below recursively constructs a Term based on a
%   node's Value dictionary. Mapping the arguments to value terms
%   results in recursion. This suggests that each argument could, in turn,
%   map to a sub-compound. The logic accommodates this possibility, although
%   such may not practically exist within a justification tree. Generally,
%   arguments will be atoms or possibly numbers.

value_term(Value, Term), is_dict(Value) => dict_term(Value, Term).
value_term(Value, List), is_list(Value) => maplist(value_term, Value, List).

dict_term(Value, Var), _{type:"var",
                         value:Var} :< Value => true.
dict_term(Value, Atom), _{type:"atom",
                          value:String} :< Value => atom_string(Atom, String).
dict_term(Value, Value), _{type:"number",
                           value:Value} :< Value => true.
dict_term(Value, Rational), _{type:"rational",
                              numerator:Numerator,
                              denominator:Denominator} :< Value =>
    rational(Rational, Numerator, Denominator).
dict_term(Value, Term), _{type:"compound",
                          functor:Functor,
                          args:Args} :< Value =>
    atom_string(Functor_, Functor),
    maplist(value_term, Args, Args_),
    Term =.. [Functor_|Args_].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

indent(Options0, Width, '~t~*|'-[Indent0], [indent(Indent)|Options]) :-
    select_option(indent(Indent0), Options0, Options, 0),
    Indent is Indent0 + Width.

indent(Options0, Tab, Options) :-
    setting(tab, DefaultWidth),
    option(tab(Width), Options0, DefaultWidth),
    indent(Options0, Width, Tab, Options).
