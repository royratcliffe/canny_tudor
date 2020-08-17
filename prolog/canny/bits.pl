:- module(canny_bits, [bits/5, bits/4]).

:- use_module(library(clpfd)).

bits_(Shift, Width, Word0, Word, Remainder) :-
    Word0_ #= Word0 >> Shift,
    Word_ #= Word >> Shift,
    bits(Width, Word0_, Word_, Remainder_),
    Remainder #= Remainder_ << Shift.

bits(Width, Word0, Word, Rest) :-
    Mask #= 1 << Width - 1,
    Word #= Word0 /\ Mask,
    Rest #= Word0 xor Word,
    Word0 #= Word /\ Mask \/ Rest.

bits(Shift, Width, Word0, Word, Rest) :-
    Mask #= (1 << Width - 1) << Shift,
    Word_ #= Word0 /\ Mask,
    Rest #= Word0 xor Word_,
    Word0 #= Word_ /\ Mask \/ Rest,
    Word #= Word_ >> Shift.
