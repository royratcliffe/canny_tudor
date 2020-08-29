:- module(canny_endian, [byte//1, big_endian//2, little_endian//2]).

/** <module> Big- and little-endian grammars
 *
 * The endian predicates unify big- and   little-endian words, longs and
 * long words with lists of  octets  by   applying  shifts  and masks to
 * correctly align integer  values  with   their  endian-specific  octet
 * positions. They utilise  integer-relational   finite  domain  CLP(FD)
 * predicates in order to  implement   forward  and  reverse translation
 * between octets and integers.
 *
 * Use of CLP allows the DCG clauses   to  express the integer relations
 * between octets and their  integer   interpretations  implicitly.  The
 * constraints simultaneously define a byte  in   terms  of an octet and
 * vice versa.
 *
 */

:- use_module(library(clpfd)).

%!  byte(?Byte:integer)// is semidet.
%
%   Parses or generates an octet for Byte. Bytes are eight bits wide and
%   unify with octets between 0  and   255  inclusive.  Fails for octets
%   falling outside this valid range.
%
%   @arg Byte value of octet.

byte(Byte) -->
    { Byte #= Octet /\ 0xff,
      Octet #= Byte /\ 0xff
      % Unifies a byte with an octet and vice versa. Succeeds only when the
      % octet fits within eight bits and so also the byte. Octets outside the
      % range 0 through 255 inclusive fail to match the byte size requirements.
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
%
%   Unifies High and Low with Word over Width bits.
%
%   Matches High and Low with Word depending on endian-order. Big-endian
%   words map to octets where  the   most-significant  precede the least
%   significant.  Little-endian  words   reverse    this   ordering.  By
%   convention, many network-oriented octet   streams  prefer big-endian
%   because it reads more naturally when you dump the octets in progress
%   order.
%
%   @arg Width of Word: 16, 32 or 64 bits.
%   @arg High half of Word.
%   @arg Low half of Word.
%   @arg Word value of one or more octets.

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
