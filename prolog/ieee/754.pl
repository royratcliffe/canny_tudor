:- module(ieee_754, [ieee_754_float/3]).

:- use_module(library(clpfd)).

:- use_module(library(canny/maths)).
:- use_module(library(canny/bits)).

ieee(32, 8, 127).
ieee(64, 11, 1023).

ieee_754_float(Bits, Word, Float) :-
    var(Float),
    !,
    sig_exp(Bits, Word, Sig, Exp),
    ldexp(Sig, Float, Exp).
ieee_754_float(Bits, Word, Float) :-
    frexp(Float, Sig, Exp),
    sig_exp_(Bits, Word, Sig, Exp).

sig_exp_(Bits, 0, 0.0, 0) :- ieee(Bits, _, _), !.
sig_exp_(Bits, Word, Sig, Exp) :- sig_exp(Bits, Word, Sig * 2, Exp - 1).

sig_exp(Bits, Word, Sig, Exp) :-
    ieee(Bits, ExpBits, ExpBias),
    SigBits is Bits - ExpBits - 1,
    bits(Bits - 1, 1, Word, Sign, Word1),
    bits(0, SigBits, Word1, Sig0, Word2),
    bits(SigBits, ExpBits, Word2, Exp0, 0),
    Exp #= Exp0 - ExpBias,
    sig(Sign, 1 << SigBits, Sig0, Sig).

sig(0, X, Sig0, Sig) :- var(Sig), !, Sig is Sig0 / X + 1.
sig(1, X, Sig0, Sig) :- var(Sig), !, Sig is -(Sig0 / X + 1).
sig(1, X, Sig0, Sig) :- sign(Sig) < 0, !, Sig0 is round((-Sig - 1) * X).
sig(0, _, 0, Sig) :- 0 is round(Sig), !.
sig(0, X, Sig0, Sig) :- Sig0 is round((Sig - 1) * X).
