:- module(endian, [byte//1, big_endian//2, little_endian//2]).

/** <module> Big- and little-endian grammars
 *
 */

:- use_module(library(clpfd)).

%!  byte(?Byte:integer)// is semidet.

byte(Byte) -->
    { Byte #= Octet /\ 0xff,
      Octet #= Byte /\ 0xff
    },
    [Octet].

%!  big_endian(?Width:integer, ?Word:integer)// is semidet.

big_endian(16, Word16) -->
    { high_low(16, High, Low, Word16)
    },
    byte(High),
    byte(Low).
big_endian(32, Word32) -->
    { high_low(32, High, Low, Word32)
    },
    big_endian(16, High),
    big_endian(16, Low).
big_endian(64, Word64) -->
    { high_low(64, High, Low, Word64)
    },
    big_endian(32, High),
    big_endian(32, Low).

%!  little_endian(?Width:integer, ?Word:integer)// is semidet.

little_endian(16, Word16) -->
    { high_low(16, High, Low, Word16)
    },
    byte(Low),
    byte(High).
little_endian(32, Word32) -->
    { high_low(32, High, Low, Word32)
    },
    little_endian(16, Low),
    little_endian(16, High).
little_endian(64, Word64) -->
    { high_low(64, High, Low, Word64)
    },
    little_endian(32, Low),
    little_endian(32, High).

%!  high_low(?Width:integer,
%!           ?High:integer,
%!           ?Low:integer,
%!           ?Word:integer) is semidet.

high_low(16, High, Low, Word16) :-
    Low #= Word16 /\ 0xff,
    High #= (Word16 >> 8) /\ 0xff,
    Word16 #= (High << 8) \/ Low.
high_low(32, High, Low, Word32) :-
    Low #= Word32 /\ 0xffff,
    High #= (Word32 >> 16) /\ 0xffff,
    Word32 #= (High << 16) \/ Low.
high_low(64, High, Low, Word64) :-
    Low #= Word64 /\ 0xffff_ffff,
    High #= (Word64 >> 32) /\ 0xffff_ffff,
    Word64 #= (High << 32) \/ Low.
