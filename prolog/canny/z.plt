:- begin_tests(canny_z).
:- use_module(z).

test(comment) :-
    setup_call_cleanup(
        tmp_file(test, ZipFile),
        (   new_memory_file(MemFile),
            enz([zip(hello, zip{comment:world}, MemFile)], ZipFile),
            unz(ZipFile, [zip(hello, Info, _)]),
            zip{comment:"world"} :< Info
        ),
        delete_file(ZipFile)
    ).

:- end_tests(canny_z).
