:- module(canny_bits, [bits/5, bits/4, bits/3]).

:- use_module(library(clpfd)).

bits(Shift, Width, Word, Bits, Rest) :-
    Mask #= (1 << Width - 1) << Shift,
    Bits0 #= Word /\ Mask,
    Rest #= Word xor Bits0,
    Word #= Bits0 /\ Mask \/ Rest,
    Bits #= Bits0 >> Shift,
    Bits0 #= Bits << Shift.

bits(Shift-Width, Word, Bits, Rest) :- bits(Shift, Width, Word, Bits, Rest).

bits(Shift-Width, Word, Bits) :- bits(Shift, Width, Word, Bits, _Rest).
