:- module(ieee_754, [ieee_754_float/3]).

:- use_module(library(clpfd)).

:- use_module(library(canny/maths)).
:- use_module(library(canny/bits)).

%!  ieee(Width, ExpWidth, ExpBias) is semidet.
%
%   IEEE 754 has, for  floating-point  numbers   of  Width  wide, ExpWidth
%   exponent bits with bias of ExpBias. The bias applies to the integral
%   base-2 exponent and determines its zero value.
%
%   Supports binary formats only. Does *not* support decimal formats.

ieee(16, 5, 15).
ieee(32, 8, 127).
ieee(64, 11, 1023).
ieee(128, 15, 16383).
ieee(256, 19, 262143).

%!  inf(Width, Inf) is semidet.
%
%   Infinity has all exponent bits set and  a zero significand. IEEE 754
%   distinguishes between positive and negative  infinity using the sign
%   bit.

inf(Width, Inf) :-
    ieee(Width, ExpWidth, _),
    Inf is ((1 << ExpWidth) - 1) << (Width - ExpWidth - 1).

%!  ieee_754_float(+Width, ?Word, ?Float) is det.
%!  ieee_754_float(-Width, ?Word, ?Float) is nondet.
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

ieee_754_float(Width, Word, Float) :-
    var(Float),
    !,
    sig_exp(Width, Word, Sig, Exp),
    ldexp(Sig, Float, Exp).
ieee_754_float(Width, 0, 0.0) :- ieee(Width, _, _), !.
ieee_754_float(Width, Inf, +1.0Inf) :- !, inf(Width, Inf).
ieee_754_float(Width, Inf, -1.0Inf) :-
    !,
    inf(Width, Inf0),
    Inf is 1 << (Width - 1) \/ Inf0.
ieee_754_float(Width, NaN, 1.5NaN) :-
    !,
    inf(Width, Inf0),
    ieee(Width, ExpWidth, _),
    NaN is Inf0 \/ (1 << (Width - ExpWidth - 2)).
ieee_754_float(Width, Word, Float) :-
    frexp(Float, Sig, Exp),
    sig_exp(Width, Word, Sig * 2, Exp - 1).

sig_exp(Width, Word, Sig, Exp) :-
    ieee(Width, ExpWidth, ExpBias),
    SigWidth is Width - ExpWidth - 1,
    bits(Width - 1, 1, Word, Sign, Word1),
    bits(0, SigWidth, Word1, Word0, Word2),
    bits(SigWidth, ExpWidth, Word2, Exp0, 0),
    Exp #= Exp0 - ExpBias,
    sig(Sign, Word0, 1 << SigWidth, Sig).

sig(Sign, Word, Max, Sig) :- var(Sig), !, ieee_sig(Sign, Word, Max, Sig).
sig(Sign, Word, Max, Sig) :- sig_ieee(Sign, Sig, Max, Word).

ieee_sig(0, Word, Max, Sig) :- ieee_sig(Word, Max, Sig), !.
ieee_sig(1, Word, Max, Sig) :- ieee_sig(Word, Max, Sig0), Sig is -Sig0.

ieee_sig(Word, Max, Sig) :- Sig is Word / Max + 1.

sig_ieee(1, Sig, Max, Word) :- sign(Sig) < 0, !, sig_ieee(-Sig, Max, Word).
sig_ieee(0, Sig, Max, Word) :- sig_ieee(Sig, Max, Word).

sig_ieee(Sig, Max, Word) :- Word is round((Sig - 1) * Max).
