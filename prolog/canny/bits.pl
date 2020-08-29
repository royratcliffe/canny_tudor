:- module(canny_bits, [bits/5, bits/4, bits/3]).

:- use_module(library(clpfd)).

%!  bits(+Shift, +Width, ?Word, ?Bits, ?Rest) is semidet.
%
%   Unifies Bits within a Word using Shift  and Width. All arguments are
%   integers treated as words of arbitrary bit-width.
%
%   The implementation uses relational integer arithmetic, i.e. CLP(FD).
%   Hence allows for forward and backward   transformations from Word to
%   Bits and vice versa. Integer Word applies a Shift and bit Width mask
%   to integer Bits. Bits is always   a  smaller integer. Decomposes the
%   problem  into  shifting  and  masking.    Treats   these  operations
%   separately.
%
%   @arg Width of Bits from  Word  after   Shift.  Width  of zero always
%   fails.

bits(Shift, Width, Word, Bits, Rest) :-
    Mask #= (1 << Width - 1) << Shift,
    Bits0 #= Word /\ Mask,
    Rest #= Word xor Bits0,
    Word #= Bits0 /\ Mask \/ Rest,
    Bits #= Bits0 >> Shift,
    Bits0 #= Bits << Shift.

bits(Shift-Width, Word, Bits, Rest) :- bits(Shift, Width, Word, Bits, Rest).

bits(Shift-Width, Word, Bits) :- bits(Shift, Width, Word, Bits, _Rest).
