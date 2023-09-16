:- begin_tests(canny_shifter).
:- use_module(shifter).

test(bit_shift, [true(A==256)]) :- bit_shift(8, A, 1).
test(bit_shift, [true(A==1)]) :- bit_shift(8, 256, A).
test(bit_shift, [true(A==255)]) :- bit_shift(15:8, 16'FF_00, A).

:- end_tests(canny_shifter).
