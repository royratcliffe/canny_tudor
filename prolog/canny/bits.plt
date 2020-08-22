:- begin_tests(canny_bits).

:- use_module(bits).

test(bits, [true(A==252)]) :- bits(0-2, 255, 3, A).
test(bits, [true(A==2'10101000)]) :- bits(0-2, 2'1010 1010, 2'10, A).

test(bits, [true(A-B==0xf-2'1100 0011)]) :- bits(2, 4, 0xff, A, B).

test(bits, [true(A-B==1-0)]) :- bits(31, 1, 1<<31, A, B).
test(bits, [true(A==0x8000_0000)]) :- bits(31, 1, A, 1, 0).

:- end_tests(canny_bits).
