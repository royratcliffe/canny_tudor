:- begin_tests(canny_octet).
:- use_module(octet).

test(octet_bits, [A-B == 10-11]) :- octet_bits(0xab, [A:4, B:4]).
test(octet_bits, [A == 16'11]) :- octet_bits(A, [1:4, 1:4]).
test(octet_bits, A == 1) :- octet_bits(16'11, [1:4, A:4]).

:- end_tests(canny_octet).
