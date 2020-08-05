:- module(endian, [byte//1, big//2, little//2]).

:- use_module(library(clpfd)).

byte(Byte) -->
    { Byte #= Octet /\ 0xff,
      Octet #= Byte /\ 0xff
    },
    [Octet].

high_low_word(High, Low, Word) :-
    Low #= Word /\ 0xff,
    High #= (Word >> 8) /\ 0xff,
    Word #= (High << 8) \/ Low.

high_low_long(High, Low, Long) :-
    Low #= Long /\ 0xffff,
    High #= (Long >> 16) /\ 0xffff,
    Long #= (High << 16) \/ Low.

high_low_long_word(High, Low, LongWord) :-
    Low #= LongWord /\ 0xffff_ffff,
    High #= (LongWord >> 32) /\ 0xffff_ffff,
    LongWord #= (High << 32) \/ Low.

big(word, Word) -->
    { high_low_word(High, Low, Word) },
    byte(High),
    byte(Low).
big(long, Long) -->
    { high_low_long(High, Low, Long) },
    big(word, High),
    big(word, Low).
big(long_word, LongWord) -->
    { high_low_long_word(High, Low, LongWord) },
    big(long, High),
    big(long, Low).

little(word, Word) -->
    { high_low_word(High, Low, Word) },
    byte(Low),
    byte(High).
little(long, Long) -->
    { high_low_long(High, Low, Long) },
    little(word, Low),
    little(word, High).
little(long_word, LongWord) -->
    { high_low_long_word(High, Low, LongWord) },
    little(long, Low),
    little(long, High).
