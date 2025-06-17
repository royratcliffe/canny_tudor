/*  File:    scasp_just_dot.pl
    Author:  Roy Ratcliffe
    Created: Jun  8 2025
    Purpose: Generate a DOT graph from a JSON source produced by s(CASP)
    License: MIT License
    Version: 1.0.0
    Status:  Stable
    Updated: 2025-06-08

Copyright (c) 2025, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
permit persons to whom the Software is   furnished  to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

:- module(scasp_just_dot,
          [ scasp_just_dot_print/3              % Stream, Src, Options
          ]).
:- autoload(library(http/json), [json_read_dict/2]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(option), [select_option/4, option/3]).
:- autoload(library(dcg/high_order), [sequence//2]).
:- use_module(library(settings), [setting/4, setting/2]).

:- setting(tab, nonneg, 2, '').
:- setting(rankdir, atom, 'LR', '').
:- setting(bgcolor, atom, transparent, '').
:- setting(node, list(compound), [ style=filled,
                                   fillcolor=lightyellow,
                                   color=darkred,
                                   fontname="Arial",
                                   fontsize=10
                                 ], '').
:- setting(edge, list(compound), [color=darkred], '').

%!  scasp_just_dot_print(+Stream, +Src, +Options) is det.
%
%   Reads a JSON file from Src, which is expected to be in the format
%   produced by the s(CASP) solver, and prints a DOT representation of the
%   justification graph to the specified Stream. The Options parameter
%   allows customisation of the output, such as indentation size, graph
%   direction, background colour, node attributes, edge attributes, and
%   nodes to elide.
%
%   The JSON source should contain a dictionary with the following structure,
%   simplified for clarity:
%
%   ```
%   {
%       "solver": {...},
%       "query": {...},
%       "answers": [
%           {
%               "bindings": {...},
%               "model": [{"truth": ..., "value": {...}}],
%               "tree": {
%                   "node": {"value": {...}},
%                   "children": [
%                       {
%                           "node": {"value": {...}},
%                           "children": [...]
%                       },
%                       ...
%                   ]
%               }
%           },
%           ...
%       ]
%   }
%   ```
%
%   The `answers` field is a list of answers, each containing bindings, a
%   model, and a tree structure. The `tree` field represents the
%   justification tree, where each node has a value and may have children,
%   forming a hierarchical structure of implications.
%
%   The output is a DOT graph representation of the justification tree,
%   where each node corresponds to a term in the justification, and edges
%   represent implications between nodes. The graph is directed, with arrows
%   indicating the direction of implications from one node to another.
%
%   The output is formatted as a DOT graph, which can be visualised using
%   graph visualisation tools like Graphviz. The output can be customised
%   using the Options parameter, which allows for setting various attributes
%   of the graph, such as:
%
%     - `tab(Width)`: Specifies the indentation width for the output.
%     - `rankdir(Direction)`: Sets the direction of the graph layout, e.g.
%       'LR' for left-to-right.
%     - `bgcolor(Color)`: Sets the background colour of the graph.
%     - `node(Attributes)`: Specifies attributes for the nodes in the graph.
%     - `edge(Attributes)`: Specifies attributes for the edges in the graph.
%     - `elides(Nodes)`: A list of nodes to elide in the graph, meaning they
%       will not be displayed.
%
%   This predicate is useful for visualising the justification structure of
%   s(CASP) queries, making it easier to understand the relationships
%   between different terms and their implications in the context of logic
%   programming and answer set programming.

scasp_just_dot_print(Stream, Src, Options) :-
    read_json_dict(Src, Dict),
    phrase(json_dot(Dict, Options), Lines),
    print_message_lines(Stream, '', Lines).

read_json_dict(Src, Dict) :-
    setup_call_cleanup(open(Src, read, Stream),
                       json_read_dict(Stream, Dict),
                       close(Stream)).

json_dot(Dict, Options) -->
    tab_dot(Options), ['// solver ~s'-[Dict.solver], nl],
    tab_dot(Options), ['// query '], value_w(Dict.query), [nl],
    line_dot('digraph {', Options, Options_),
    answers_dot(Dict.answers, Options_),
    line_dot('}', Options).

answers_dot(Answers, Options) -->
    setting_dot(rankdir, Options),
    setting_dot(bgcolor, Options),
    setting_dot(node, Options),
    setting_dot(edge, Options),
    sequence(answer_dot(Options), Answers).

answer_dot(Options, Answer) -->
    dict_comment_dot(Answer, [excludes([ bindings,
                                         model,
                                         constraints,
                                         tree
                                       ])|Options]),
    { dict_pairs(Answer.bindings, _, Bindings)
    },
    sequence(binding_comment_dot(Options), Bindings),
    sequence(truth_comment_dot(Options), Answer.model),
    answer_tree_dot(Answer.tree, Options).

answer_tree_dot(_{node:Node, children:Children}, Options) -->
    { value_term(Node.value, query)
    },
    line_dot('subgraph {', Options, Options_),
    sequence(answer_tree_query_dot([], Options_), Children),
    line_dot('}', Options).

answer_tree_query_dot(Nodes, Options, Answer) -->
    { value_term(Answer.node.value, Node)
    },
    line_dot('// ~w'-[Node], Options),
    line_dot('subgraph {', Options, Options_),
    sequence(implies_dot(Node, Options_), Nodes),
    sequence(answer_tree_query_dot([Node|Nodes], Options_), Answer.children),
    line_dot('}', Options).

%!  implies_dot(+Node0, +Options, +Node)// is det.
%
%   Generates a DOT graph representation of an implication from Node0 to Node.
%
%   If Node0 is not in the list of elided nodes, it generates a line indicating
%   the implication from Node0 to Node. If Node0 is in the list of elided nodes,
%   it generates a comment line indicating that the implication is elided.
%
%   The elision is controlled by the `elides/1` option in Options, which is a
%   list of nodes that should *not* be displayed in the graph.
%
%   The predicate constructs a DOT line in the format:
%   ```
%   "// elided " if Node0 or Node is in the elides list
%   "~s"~w" -> "~w";" if neither Node0 nor Node is elided
%   ```
%   where `~s` is replaced by the elided comment if applicable, and the `~w`
%   format string are replaced by the string representations of Node0 and Node.
%
%   The predicate uses the `line_dot//2` DCG rule to format the output line
%   according to the DOT syntax, including the optional comment if the node is
%   elided. The `Elides` variable is used to check if either Node0 or Node is in
%   the list of elided nodes. If neither is elided, it generates a standard DOT
%   line containing the implication. If either is elided, it generates a comment
%   line indicating that the implication is elided.

implies_dot(Node0, Options, Node) -->
    { option(elides(Elides), Options, []),
      (   \+ memberchk(Node0, Elides),
          \+ memberchk(Node, Elides)
      ->  Elided = ""
      ;   Elided = "// elided "
      )
    },
    line_dot('~s"~w" -> "~w";'-[Elided, Node0, Node], Options).

binding_comment_dot(Options, Binding-Truth) -->
    tab_dot(Options), ['// ~w: '-[Binding]], truth_w(Truth), [nl].

truth_comment_dot(Options, Truth) -->
    tab_dot(Options), ['// '], truth_w(Truth), [nl].

dict_comment_dot(Dict, Options) -->
    { dict_pairs(Dict, _, Pairs)
    },
    sequence(pair_comment_dot(Options), Pairs).

pair_comment_dot(Options, Key-Value) -->
    { includes_or_not_excludes(Key, Options)
    },
    !,
    line_dot('// ~k ~k'-[Key, Value], Options).
pair_comment_dot(_, _) --> [].

%!  includes_or_not_excludes(+Key, +Options) is semidet.
%
%   Determines if a Key should be included in the output based on the provided
%   Options. Succeeds if Key is included (if includes/1 is present), not
%   excluded (if excludes/1 is present), or always succeeds if neither option is
%   present.
%
%   If the includes/1 option is present, it checks if Key is a member of the
%   Includes list. If the excludes/1 option is present, it checks that Key is
%   not a member of the Excludes list.

includes_or_not_excludes(Key, Options) :-
    (   option(includes(Includes), Options)
    ->  memberchk(Key, Includes)
    ;   option(excludes(Excludes), Options)
    ->  \+ memberchk(Key, Excludes)
    ;   true % succeed if neither includes nor excludes (default behaviour)
    ).

setting_dot(Name, Options) -->
    { setting(Name, Value)
    },
    setting_dot(Name, Value, Options).

setting_dot(Name, Value, Options) -->
    { is_list(Value)
    },
    !,
    line_dot('~p ~p;'-[Name, Value], Options).
setting_dot(Name, Value, Options) -->
    line_dot('~w;'-[Name=Value], Options).

tab_dot(Options0, Options) -->
    { indent(Options0, Tab, Options)
    },
    [Tab].

tab_dot(Options) --> tab_dot(Options, _).

line_dot(Line, Options0, Options) --> tab_dot(Options0, Options), [Line, nl].

line_dot(Line, Options) --> line_dot(Line, Options, _).

truth_w(_{truth:Truth, value:Value}) -->
    { value_term(Value, Term)
    },
    ['~w ~w'-[Truth, Term]].

value_p(Value) -->
    { value_term(Value, Term)
    },
    ['~p'-[Term]].

value_w(Value) -->
    { value_term(Value, Term)
    },
    ['~w'-[Term]].

%!  value_term(+Value:dict, -Term) is semidet.
%
%   Converts a Prolog dictionary (parsed from JSON) that represents a term into
%   an actual Prolog term. The predicate deals with cases where the dictionary
%   represents different types of terms, such as variables, atoms, numbers,
%   rational numbers, and compound terms. It recursively processes the arguments
%   of compound terms to construct the final term. The predicate handles the
%   following cases:
%
%     - If the input is a dictionary representing a variable, it extracts the
%       variable name and returns it as a Prolog *string*.
%     - If the dictionary represents an atom, it converts the string value to
%       a Prolog atom.
%     - If the dictionary represents a number, it returns the number as is.
%     - If the dictionary represents a rational number, it constructs a
%       rational term from the numerator and denominator.
%     - If the input is a dictionary representing a compound term, it
%       recursively processes the arguments and constructs the compound term
%       using the functor and the arguments.
%
%   The predicate employs dictionary pattern matching (`:<`) to safely extract
%   the relevant fields from the dictionary, and for clarity. It handles nested
%   compounds by recursively mapping the arguments to value terms. The predicate
%   is semi-deterministic, meaning it succeeds at most once; if the input
%   dictionary does not match any of the expected patterns, it fails silently.
%   This allows for safe handling of the JSON structure without raising
%   exceptions for unexpected formats.
%
%   The implementation assumes that the JSON structure follows a specific
%   format, where each term is represented as a dictionary with a `type`
%   field indicating whether it is a variable, atom, number, rational, or
%   compound. If the JSON structure changes or new term types are added,
%   the predicate will require updating.
%
%   The `value_term/2` predicate is used to convert the JSON representation
%   of terms into Prolog terms, which can then be used in further processing
%   or output---particularly useful in the context of generating a DOT
%   graph from a JSON source produced by s(CASP), where terms represent nodes
%   in the justification tree. It is a crucial part of justification tree
%   processing, allowing the conversion of JSON representations of terms into
%   Prolog terms that can be used to construct the graph structure, and designed
%   to be used in conjunction with the `dict_term/2` helper predicate, which
%   handles the conversion of a Prolog dictionary (parsed from JSON) into a
%   Prolog term. The `dict_term/2` predicate is responsible for converting the
%   dictionary representation of terms into actual Prolog terms.

value_term(Value, Term), is_dict(Value) => dict_term(Value, Term).
value_term(Value, List), is_list(Value) => maplist(value_term, Value, List).

dict_term(Value, Var), _{type:"var",
                         name:Var} :< Value => true.
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

%!  indent(+Options0:list, +Width:nonneg, -Tab:compound, -Options:list) is det.
%
%   Computes the next indentation level and returns a tabulation format for output.
%   - Options0: Input options list, possibly containing indent/1.
%   - Width: Amount to increase the indentation by.
%   - Tab: Tabulation format for output (i.e., '~t~*|'-[Indent0]).
%   - Options: Output options list with updated indent/1 option.

indent(Options0, Width, '~t~*|'-[Indent0], [indent(Indent)|Options]) :-
    select_option(indent(Indent0), Options0, Options, 0),
    Indent is Indent0 + Width.

indent(Options0, Tab, Options) :-
    setting(tab, DefaultWidth),
    option(tab(Width), Options0, DefaultWidth),
    indent(Options0, Width, Tab, Options).
