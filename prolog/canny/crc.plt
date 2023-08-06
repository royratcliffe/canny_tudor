:- begin_tests(canny_crc).

:- use_module(crc).

test(poly_degree) :-
    crc:poly_degree(16'107, 8),
    crc:poly_degree(16'18005, 16),
    crc:poly_degree(16'1864CFB, 24),
    crc:poly_degree(16'104C11DB7, 32),
    crc:poly_degree(16'1AD93D23594C935A9, 64).

:- end_tests(canny_crc).
