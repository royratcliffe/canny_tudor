:- begin_tests(canny_bits).

:- use_module(bits).

test(bits, [true(A==252)]) :- bits(0-2, 255, 3, A).
test(bits, [true(A==2'10101000)]) :- bits(0-2, 2'1010 1010, 2'10, A).

test(bits, [true(A-B==0xf-2'1100 0011)]) :- bits(2, 4, 0xff, A, B).

test(bits, [true(A-B==1-0)]) :- bits(31, 1, 1<<31, A, B).
test(bits, [true(A==0x8000_0000)]) :- bits(31, 1, A, 1, 0).

test(bit_fields, true(A-B == 3-3)) :- bit_fields([A:2, B:2], 4, 2'1111).
test(bit_fields, true(A == 2'0101)) :- bit_fields([1:2, 1:2], 4, 0, A).

test(rbit, [true(A==16'8000_0000)]) :-
    rbit(32, 1, A).
test(rbit, [true(A==16'80_00)]) :-
    rbit(16, 1, A).
test(rbit, [true(A==16'8408)]) :-
    rbit(16, 16'1_1021, A).

:- end_tests(canny_bits).
