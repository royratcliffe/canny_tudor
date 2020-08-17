:- begin_tests(canny_bits).

:- use_module(bits).

test(bits, [true(A==252)]) :- bits(2, 255, 3, A).
test(bits, [true(A==2'10101000)]) :- bits(2, 2'1010 1010, 2'10, A).

test(bits, [true(A-B==0xf-2'1100 0011)]) :- bits(2, 4, 0xff, A, B).

:- end_tests(canny_bits).
