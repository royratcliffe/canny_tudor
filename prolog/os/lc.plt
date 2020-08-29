:- begin_tests(os_lc).

:- use_module(lc, []).

atom_count(Atom, Count) :-
    setup_call_cleanup(
        atom_to_memory_file(Atom, File),
        setup_call_cleanup(
            open_memory_file(File, read, Stream),
            os_lc:stream_count(Stream, Count),
            close(Stream)),
        free_memory_file(File)).

test(stream_count, true(A==0)) :- atom_count('', A).
test(stream_count, true(A==1)) :- atom_count('\n', A).
test(stream_count, true(A==2)) :- atom_count('a\nb\n', A).
test(stream_count, true(A==3)) :- atom_count('hello\nworld\n!', A).

:- end_tests(os_lc).
