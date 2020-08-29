:- module(ieee_754, [ieee_754_float/3]).

:- use_module(library(clpfd)).

:- use_module(library(canny/maths)).
:- use_module(library(canny/bits)).

%!  ieee(Bits, ExpBits, ExpBias) is semidet.
%
%   IEEE 754 has, for  floating-point  numbers   of  Bits  wide, ExpBits
%   exponent bits with bias of ExpBias. The bias applies to the integral
%   base-2 exponent and determines its zero value.
%
%   Supports binary formats only. Does *not* support decimal formats.

ieee(16, 5, 15).
ieee(32, 8, 127).
ieee(64, 11, 1023).
ieee(128, 15, 16383).
ieee(256, 19, 262143).

%!  inf(Bits, Inf) is semidet.
%
%   Infinity has all exponent bits set and  a zero significand. IEEE 754
%   distinguishes between positive and negative  infinity using the sign
%   bit.

inf(Bits, Inf) :-
    ieee(Bits, ExpBits, _),
    Inf is ((1 << ExpBits) - 1) << (Bits - ExpBits - 1).

%!  ieee_754_float(+Bits, ?Word, ?Float) is det.
%!  ieee_754_float(-Bits, ?Word, ?Float) is nondet.
%
%   Performs two-way pack and unpack for IEEE 754 floating-point numbers
%   represented as words.
%
%   Not designed for performance. Uses CLP(FD) for bit manipulation. and
%   hence remains within the integer   domain.  Float arithmetic applies
%   outside the finite-domain constraints.
%
%   @arg Word is a non-negative integer.   This  implementation does not
%   handle negative integers. Negative support implies a non-determinate
%   solution for packing. A positive and  negative answer exists for any
%   given Float.
%
%   @arg Sig is the floating-point significand between plus and minus 1.
%   Uses Sig rather than Mantissa; Sig   short  for Significand, another
%   word for mantissa.

ieee_754_float(Bits, Word, Float) :-
    var(Float),
    !,
    sig_exp(Bits, Word, Sig, Exp),
    ldexp(Sig, Float, Exp).
ieee_754_float(Bits, 0, 0.0) :- ieee(Bits, _, _), !.
ieee_754_float(Bits, Inf, +1.0Inf) :- !, inf(Bits, Inf).
ieee_754_float(Bits, Inf, -1.0Inf) :-
    !,
    inf(Bits, Inf0),
    Inf is 1 << (Bits - 1) \/ Inf0.
ieee_754_float(Bits, NaN, 1.5NaN) :-
    !,
    inf(Bits, Inf0),
    ieee(Bits, ExpBits, _),
    NaN is Inf0 \/ (1 << (Bits - ExpBits - 2)).
ieee_754_float(Bits, Word, Float) :-
    frexp(Float, Sig, Exp),
    sig_exp(Bits, Word, Sig * 2, Exp - 1).

sig_exp(Bits, Word, Sig, Exp) :-
    ieee(Bits, ExpBits, ExpBias),
    SigBits is Bits - ExpBits - 1,
    bits(Bits - 1, 1, Word, Sign, Word1),
    bits(0, SigBits, Word1, Word0, Word2),
    bits(SigBits, ExpBits, Word2, Exp0, 0),
    Exp #= Exp0 - ExpBias,
    sig(Sign, Word0, 1 << SigBits, Sig).

sig(Sign, Word, Max, Sig) :- var(Sig), !, ieee_sig(Sign, Word, Max, Sig).
sig(Sign, Word, Max, Sig) :- sig_ieee(Sign, Sig, Max, Word).

ieee_sig(0, Word, Max, Sig) :- ieee_sig(Word, Max, Sig), !.
ieee_sig(1, Word, Max, Sig) :- ieee_sig(Word, Max, Sig0), Sig is -Sig0.

ieee_sig(Word, Max, Sig) :- Sig is Word / Max + 1.

sig_ieee(1, Sig, Max, Word) :- sign(Sig) < 0, !, sig_ieee(-Sig, Max, Word).
sig_ieee(0, Sig, Max, Word) :- sig_ieee(Sig, Max, Word).

sig_ieee(Sig, Max, Word) :- Word is round((Sig - 1) * Max).
