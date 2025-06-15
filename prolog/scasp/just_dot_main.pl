:- use_module(just_dot).
:- use_module(library(main)).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Positional, Options, []),
    forall(member(Src, Positional), json_to_dot(Src, Options)).

json_to_dot(Src, Options) :-
    file_name_extension(Base, json, Src),
    file_name_extension(Base, dot, Dest),
    setup_call_cleanup(open(Dest, write, Out),
                       scasp_print_just_dot(Out, Src, Options),
                       close(Out)).
