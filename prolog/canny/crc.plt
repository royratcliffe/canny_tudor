:- begin_tests(canny_crc).

:- use_module(crc).

cat_mouse_987654321(crc-8, 16'D5).
cat_mouse_987654321(crc-8-itu, 16'80).
cat_mouse_987654321(crc-16-mcrf4xx, 16'F56E).
cat_mouse_987654321(crc-32-bzip2, 16'AF53_5767).
cat_mouse_987654321(crc-64-jones, 16'7036_9BE5_4794_2213).

test(predefined_check_CatMouse987654321, [forall(cat_mouse_987654321(Predefined, Check))]) :-
    crc(Predefined, CRC0),
    crc(CRC0, `CatMouse987654321`, CRC),
    crc_property(CRC, check(Check)).

test(poly_deg) :-
    canny_crc:poly_deg(16'107, 8),
    canny_crc:poly_deg(16'18005, 16),
    canny_crc:poly_deg(16'1864CFB, 24),
    canny_crc:poly_deg(16'104C11DB7, 32),
    canny_crc:poly_deg(16'1AD93D23594C935A9, 64).

test(crc_16_mcrf4xx, true(B == 16'F56E)) :-
    crc_16_mcrf4xx(A),
    crc_16_mcrf4xx(A, `CatMouse987654321`, B).

:- end_tests(canny_crc).
