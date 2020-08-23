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
    bits(0, SigBits, Word1, Word0, Word2),
    bits(SigBits, ExpBits, Word2, Exp0, 0),
    Exp #= Exp0 - ExpBias,
    sig(Sign, 1 << SigBits, Word0, Sig).

sig(Sign, Max, Word, Sig) :- var(Sig), !, ieee_sig(Sign, Word, Max, Sig).
sig(Sign, Max, Word, Sig) :- sig_ieee(Sign, Sig, Max, Word).

ieee_sig(0, Word, Max, Sig) :- ieee_sig(Word, Max, Sig), !.
ieee_sig(1, Word, Max, Sig) :- ieee_sig(Word, Max, Sig0), Sig is -Sig0.

ieee_sig(Word, Max, Sig) :- Sig is Word / Max + 1.

sig_ieee(1, Sig, Max, Word) :- sign(Sig) < 0, !, sig_ieee(-Sig, Max, Word).
sig_ieee(0, Sig, _Max, 0) :- 0 is round(Sig), !.
sig_ieee(0, Sig, Max, Word) :- sig_ieee(Sig, Max, Word).

sig_ieee(Sig, Max, Word) :- Word is round((Sig - 1) * Max).
