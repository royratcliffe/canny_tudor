:- use_module(just_dot).
:- use_module(library(main)).

:- initialization(main, main).

opt_type(tab, tab, nonneg).
opt_type(rankdir, rankdir, atom).
opt_type(bgcolor, bgcolor, atom).
opt_type(node, node, term).
opt_type(edge, edge, term).
opt_type(elides, elides, term).

opt_help(help(header), 'Generate a DOT file from a JSON file produced by scasp').
opt_help(tab, 'Tab size for indentation').
opt_help(rankdir, 'Direction of the graph layout').
opt_help(bgcolor, 'Background colour of the graph').
opt_help(node, 'Node attributes').
opt_help(edge, 'Edge default attributes').
opt_help(elides, 'Nodes to elide in the graph').

main(Argv) :-
    argv_options(Argv, Positional, Options, []),
    forall(member(Src, Positional), json_to_dot(Src, Options)).

json_to_dot(Src, Options) :-
    file_name_extension(Base, json, Src),
    file_name_extension(Base, dot, Dest),
    setup_call_cleanup(open(Dest, write, Out),
                       scasp_just_dot_print(Out, Src, Options),
                       close(Out)).
