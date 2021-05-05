:- begin_tests(canny_files).

:- use_module(files).
:- use_module(arch).

test(absolute_directory, [fail]) :-
    absolute_directory('a/b/c', _).
test(absolute_directory, [all(A==['/a/b/c', '/a/b', '/a', /])]) :-
    absolute_directory('/a/b/c', A).

:- if(current_os(win64)).
test(absolute_directory, [all(A==['a:/a/b/c', 'a:/a/b', 'a:/a', 'a:'])]) :-
    absolute_directory('a:/a/b/c', A).
test(absolute_directory, [fail]) :-
    absolute_directory('/path/to', _).
:- endif.

:- end_tests(canny_files).
