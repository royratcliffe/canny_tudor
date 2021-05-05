:- begin_tests(canny_exe).

:- use_module(exe).
:- use_module(arch).

:- if(current_os(linux)).
test(exe, true(A-B==你好-"你好")) :-
    exe(path(tee),
        [ '/dev/stderr'
        ],
        [ stdin(atom(你好, [encoding(utf8)])),
          stdout(atom(A, [encoding(utf8)])),
          stderr(string(B, [encoding(utf8)])),
          status(exit(0))
        ]).
:- endif.

:- end_tests(canny_exe).
