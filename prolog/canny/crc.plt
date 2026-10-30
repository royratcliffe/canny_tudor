:- begin_tests(canny_crc).

:- use_module(crc).

standard_check(crc-8, 16'F4).
standard_check(crc-8-itu, 16'A1).
standard_check(crc-16-mcrf4xx, 16'6F91).
standard_check(crc-x25, 16'906E).
standard_check(crc-32, 16'CBF4_3926).
standard_check(crc-32-bzip2, 16'FC89_1918).
standard_check(crc-64-jones, 16'CAA7_1716_8609_F281).

cat_mouse_987654321(crc-8, 16'D5).
cat_mouse_987654321(crc-8-itu, 16'80).
cat_mouse_987654321(crc-16-mcrf4xx, 16'F56E).
cat_mouse_987654321(crc-32-bzip2, 16'AF53_5767).
cat_mouse_987654321(crc-64-jones, 16'7036_9BE5_4794_2213).

test(predefined_check_CatMouse987654321, [forall(cat_mouse_987654321(Predefined, Check))]) :-
    crc(Predefined, CRC0),
    crc(CRC0, `CatMouse987654321`, CRC),
    crc_property(CRC, check(Check)).

test(predefined_accumulators, true(Accumulators == [
         crc-8-crc(16'107, 0, []),
         crc-8-itu-crc(16'107, 16'55, [xor(16'55)]),
         crc-16-mcrf4xx-crc(16'11021, 16'FFFF, [reverse]),
         crc-x25-crc(16'11021, 0, [reverse, xor(16'FFFF)]),
         crc-32-crc(16'104C11DB7, 0, [reverse, xor(16'FFFFFFFF)]),
         crc-32-bzip2-crc(16'104C11DB7, 0, [xor(16'FFFFFFFF)]),
         crc-64-jones-crc(16'1AD93D23594C935A9, 16'FFFFFFFFFFFFFFFF, [reverse])
     ])) :-
    findall(Name-CRC, crc(Name, CRC), Accumulators).

test(unknown_predefined, [fail]) :-
    crc(unknown, _).

test(standard_check, [forall(standard_check(Name, Check))]) :-
    crc(Name, CRC0),
    crc(CRC0, `123456789`, CRC),
    crc_property(CRC, check(Check)).

test(properties, true(Properties == [poly(16'107), check(16'F4)])) :-
    findall(Property, crc_property(crc(16'107, 16'F4, []), Property), Properties).

test(property_mismatch, [fail]) :-
    crc_property(crc(16'107, 0, []), check(1)).

test(unknown_property, [fail]) :-
    crc_property(crc(16'107, 0, []), options(_)).

test(empty_data, [forall(standard_check(Name, _)), true(CRC == CRC0)]) :-
    crc(Name, CRC0),
    crc(CRC0, [], CRC).

test(byte_matches_singleton_list,
     [forall((standard_check(Name, _), between(0, 255, Byte))), true(CRC == ListCRC)]) :-
    crc(Name, CRC0),
    crc(CRC0, Byte, CRC),
    crc(CRC0, [Byte], ListCRC).

test(incremental_data,
     [forall((standard_check(Name, _), between(0, 9, Split))), true(CRC == WholeCRC)]) :-
    crc(Name, CRC0),
    length(Prefix, Split),
    append(Prefix, Suffix, `123456789`),
    crc(CRC0, Prefix, CRC1),
    crc(CRC1, Suffix, CRC),
    crc(CRC0, `123456789`, WholeCRC).

test(nested_data, [forall(standard_check(Name, _)), true(CRC == FlatCRC)]) :-
    crc(Name, CRC0),
    crc(CRC0, [[], `123`, [0'4, `567`], `89`], CRC),
    crc(CRC0, `123456789`, FlatCRC).

test(invalid_byte, [forall(member(Byte, [-1, 256, 65536])), fail]) :-
    crc(crc(16'107, 0, []), Byte, _).

test(invalid_data, [forall(member(Data, [invalid, 1.5, [1|2], [0, invalid], [256]])), fail]) :-
    crc(crc(16'107, 0, []), Data, _).

test(uninstantiated_data, [fail]) :-
    crc(crc(16'107, 0, []), _, _).

test(partial_list, [fail]) :-
    crc(crc(16'107, 0, []), [1|_], _).

test(uninstantiated_list_element, [fail]) :-
    crc(crc(16'107, 0, []), [_], _).

test(custom_crc_16_xmodem) :-
    crc(crc(16'11021, 0, []), `123456789`, CRC),
    crc_property(CRC, check(16'31C3)).

test(custom_crc_24_openpgp) :-
    crc(crc(16'1864CFB, 16'B704CE, []), `123456789`, CRC),
    crc_property(CRC, check(16'21CF02)).

test(zero_byte_zero_accumulator,
     [forall((member(Deg, [8, 16, 24, 32, 64]), member(Options, [[], [reverse]])))]) :-
    Poly is (1 << Deg) + 1,
    crc(crc(Poly, 0, Options), 0, crc(Poly, 0, Options)).

test(poly_deg,
     [forall(member(Poly-Deg, [
         16'107-8, 16'18005-16, 16'1864CFB-24,
         16'104C11DB7-32, 16'1AD93D23594C935A9-64
     ]))]) :-
    canny_crc:poly_deg(Poly, Deg).

test(poly_deg_boundaries, [forall(member(Deg, [8, 16, 24, 32, 64]))]) :-
    Low is 1 << Deg,
    High is (1 << (Deg + 1)) - 1,
    canny_crc:poly_deg(Low, Deg),
    canny_crc:poly_deg(High, Deg).

test(unsupported_poly,
     [forall(member(Poly, [-1, 0, 16'FF, 16'200, 16'20000, 16'20000000000000000])), fail]) :-
    canny_crc:poly_deg(Poly, _).

test(unsupported_poly_update, [fail]) :-
    crc(crc(16'200, 0, []), 0, _).

test(poly_deg_mismatch, [fail]) :-
    canny_crc:poly_deg(16'107, 16).

test(crc_16_mcrf4xx_initial, true(Check == 16'FFFF)) :-
    crc_16_mcrf4xx(Check).

test(crc_16_mcrf4xx, true(B == 16'F56E)) :-
    crc_16_mcrf4xx(A),
    crc_16_mcrf4xx(A, `CatMouse987654321`, B).

test(crc_16_mcrf4xx_standard, true(Check == 16'6F91)) :-
    crc_16_mcrf4xx(Check0),
    crc_16_mcrf4xx(Check0, `123456789`, Check).

test(crc_16_mcrf4xx_empty, true(Check == 16'ABCD)) :-
    crc_16_mcrf4xx(16'ABCD, [], Check).

test(crc_16_mcrf4xx_all_bytes,
     [forall((member(Check0, [0, 1, 16'7FFF, 16'8000, 16'FFFF]), between(0, 255, Byte))),
      true(Check == Expected)]) :-
    crc_16_mcrf4xx(Check0, Byte, Check),
    crc(crc(16'11021, Check0, [reverse]), Byte, CRC),
    crc_property(CRC, check(Expected)).

test(crc_16_mcrf4xx_all_byte_sequence,
     [forall(member(Check0, [0, 16'FFFF])), true(Check == Expected)]) :-
    numlist(0, 255, Bytes),
    crc_16_mcrf4xx(Check0, Bytes, Check),
    crc(crc(16'11021, Check0, [reverse]), Bytes, CRC),
    crc_property(CRC, check(Expected)).

test(crc_16_mcrf4xx_incremental,
     [forall(between(0, 9, Split)), true(Check == 16'6F91)]) :-
    length(Prefix, Split),
    append(Prefix, Suffix, `123456789`),
    crc_16_mcrf4xx(Check0),
    crc_16_mcrf4xx(Check0, Prefix, Check1),
    crc_16_mcrf4xx(Check1, Suffix, Check).

test(crc_16_mcrf4xx_nested, true(Check == 16'6F91)) :-
    crc_16_mcrf4xx(16'FFFF, [[], `123`, [0'4, `567`], `89`], Check).

% Unlike crc/3, the specialised updater masks integer input to its low byte.
test(crc_16_mcrf4xx_masks_byte,
     [forall(member(Data, [-1, 256, 511])), true(Check == Expected)]) :-
    Byte is Data /\ 16'FF,
    crc_16_mcrf4xx(16'FFFF, Data, Check),
    crc_16_mcrf4xx(16'FFFF, Byte, Expected).

test(crc_16_mcrf4xx_invalid_data,
     [forall(member(Data, [invalid, 1.5, [1|2], [0, invalid]])), fail]) :-
    crc_16_mcrf4xx(16'FFFF, Data, _).

test(crc_16_mcrf4xx_uninstantiated_data, [fail]) :-
    crc_16_mcrf4xx(16'FFFF, _, _).

test(crc_16_mcrf4xx_partial_list, [fail]) :-
    crc_16_mcrf4xx(16'FFFF, [1|_], _).

test(crc_16_mcrf4xx_uninstantiated_list_element, [fail]) :-
    crc_16_mcrf4xx(16'FFFF, [_], _).

:- end_tests(canny_crc).
