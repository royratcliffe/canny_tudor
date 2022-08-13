:- begin_tests(canny_octet).
:- use_module(octet).

test(octet_bits, [A-B == 10-11]) :-
    octet_bits(0xab, [A:4, B:4]).

:- end_tests(canny_octet).
