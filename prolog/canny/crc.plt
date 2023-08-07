:- begin_tests(canny_crc).

:- use_module(crc).

test(poly_deg) :-
    canny_crc:poly_deg(16'107, 8),
    canny_crc:poly_deg(16'18005, 16),
    canny_crc:poly_deg(16'1864CFB, 24),
    canny_crc:poly_deg(16'104C11DB7, 32),
    canny_crc:poly_deg(16'1AD93D23594C935A9, 64).

:- end_tests(canny_crc).
