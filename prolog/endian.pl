:- module(endian, [byte//1, big//2, little//2]).

:- use_module(library(clpfd)).

byte(Byte) -->
    { Byte #= Octet /\ 0xff,
      Octet #= Byte /\ 0xff
    },
    [Octet].

big(word, Word) -->
    { Low #= Word /\ 0xff,
      High #= (Word >> 8) /\ 0xff,
      Word #= (High << 8) \/ Low
    },
    byte(High),
    byte(Low).
big(long, Long) -->
    { Low #= Long /\ 0xffff,
      High #= (Long >> 16) /\ 0xffff,
      Long #= (High << 16) \/ Low
    },
    big(word, High),
    big(word, Low).
big(long_word, LongWord) -->
    { Low #= LongWord /\ 0xffff_ffff,
      High #= (LongWord >> 32) /\ 0xffff_ffff,
      LongWord #= (High << 32) \/ Low
    },
    big(long, High),
    big(long, Low).

little(word, Word) -->
    { Low #= Word /\ 0xff,
      High #= (Word >> 8) /\ 0xff,
      Word #= (High << 8) \/ Low
    },
    byte(Low),
    byte(High).
little(long, Long) -->
    { Low #= Long /\ 0xffff,
      High #= (Long >> 16) /\ 0xffff,
      Long #= (High << 16) \/ Low
    },
    little(word, Low),
    little(word, High).
little(long_word, LongWord) -->
    { Low #= LongWord /\ 0xffff_ffff,
      High #= (LongWord >> 32) /\ 0xffff_ffff,
      LongWord #= (High << 32) \/ Low
    },
    little(long, Low),
    little(long, High).
