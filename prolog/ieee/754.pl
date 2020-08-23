:- module(ieee_754, []).

:- use_module(library(clpfd)).

:- use_module(library(canny/maths)).
:- use_module(library(canny/bits)).

ieee(32, 8, 127).
ieee(64, 11, 1023).

pack(Width, Word, Sig, Exp) :-
    ieee(Width, ExpWidth, ExpBias),
    SigWidth is Width - ExpWidth - 1,
    bits(Width - 1, 1, Word, Sign, Word1),
    bits(0, SigWidth, Word1, Sig0, Word2),
    bits(SigWidth, ExpWidth, Word2, Exp0, 0),
    Exp #= Exp0 - ExpBias,
    sig(Sign, 1 << SigWidth, Sig0, Sig).

sig(0, X, Sig0, Sig) :- var(Sig), !, Sig is Sig0 / X.
sig(1, X, Sig0, Sig) :- var(Sig), !, Sig is -Sig0 / X.
sig(1, X, Sig0, Sig) :- Sig < 0, !, Sig0 is -round(Sig * X).
sig(0, X, Sig0, Sig) :- Sig0 is round(Sig * X).
