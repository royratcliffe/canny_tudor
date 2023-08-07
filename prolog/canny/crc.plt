:- begin_tests(canny_crc).

:- use_module(crc).

test(crc_8, true(Check == 16'D5)) :-
    crc(crc-8, CRC1),
    crc(CRC1, `CatMouse987654321`, CRC2),
    crc_property(CRC2, check(Check)).

test(crc_16_mcrf4xx, true(Check == 16'F56E)) :-
    crc(crc-16-mcrf4xx, CRC1),
    crc(CRC1, `CatMouse987654321`, CRC2),
    crc_property(CRC2, check(Check)).

test(poly_deg) :-
    canny_crc:poly_deg(16'107, 8),
    canny_crc:poly_deg(16'18005, 16),
    canny_crc:poly_deg(16'1864CFB, 24),
    canny_crc:poly_deg(16'104C11DB7, 32),
    canny_crc:poly_deg(16'1AD93D23594C935A9, 64).

:- end_tests(canny_crc).
