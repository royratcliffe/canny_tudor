/*  File:    dcg/endian.pl
    Author:  Roy Ratcliffe
    Created: Aug 28 2023
    Purpose: Big- and Little-Endian Grammars
*/

:- module(dcg_endian,
          [ endian//3,
            big_endian//2,
            little_endian//2
          ]).
:- autoload(library(dcg/basics), [remainder/3]).

%!  endian(?BigOrLittle, ?Width, ?Value)// is semidet.

endian(big, Width, Value) --> big_endian(Width, Value).
endian(little, Width, Value) --> little_endian(Width, Value).

%!  big_endian(?Width, ?Value)// is semidet.

big_endian(Width, Value) -->
    { var(Value), !
    },
    endianness(Width, Octets),
    { big_endian(Octets, 0, Value)
    }.
big_endian(Width, Value) -->
    endianness(Width, Octets),
    { big_endian_(Octets, Value, _)
    }.

big_endian([], Value, Value).
big_endian([H|T], Value0, Value) :-
    acc(H, Value0, Value_),
    big_endian(T, Value_, Value).

big_endian_([], Value, Value).
big_endian_([H|T], Value0, Value) :-
    big_endian_(T, Value0, Value_),
    acc_(H, Value_, Value).

acc(H, Value0, Value_) :-
    0 =< H,
    H =< 255,
    Value_ is H \/ (Value0 << 8).

acc_(H, Value_, Value) :-
    H is Value_ /\ 16'ff,
    Value is Value_ >> 8.

%!  little_endian(?Width, ?Value)// is semidet.

little_endian(Width, Value) -->
    { var(Value), !
    },
    endianness(Width, Octets),
    { little_endian(Octets, 0, Value)
    }.
little_endian(Width, Value) -->
    endianness(Width, Octets),
    { little_endian_(Octets, Value, _)
    }.

little_endian([], Value, Value).
little_endian([H|T], Value0, Value) :-
    little_endian(T, Value0, Value_),
    %
    %   0 =< H,
    %   H =< 255,
    %   Value is H \/ (Value_ << 8),
    %
    acc(H, Value_, Value).

little_endian_([], Value, Value).
little_endian_([H|T], Value0, Value) :-
    %
    %   H is Value0 /\ 16'ff,
    %   Value_ is Value0 >> 8,
    %
    acc_(H, Value0, Value_),
    little_endian_(T, Value_, Value).

%!  endianness(?Width, ?Octets)// is semidet.
%
%   Grammar for finding Octets by  Width.   Unites  difference  lists of
%   octet codes with _zero_ or more items by a width.
%
%   The Width term can be either a   variable or an integer. For unknown
%   widths, the clauses span the remainder  of the difference lists. The
%   length of the outstanding list of   codes determines the final width
%   multiplied by eight.
%
%   The Octets may also  have  variable   items.  The  grammar  does not
%   examine the codes themselves; it only   concerns  the length and its
%   relationship to width. The grammar fails  if   the  width is *not* a
%   multiple of eight.

endianness(Width, Octets) -->
    { var(Width), !
    },
    remainder(Octets),
    { length(Octets, Len),
      Width is Len << 3
    }.
endianness(Width, Octets) -->
    { Width_ is Width /\ 2'111,
      Width_ == 0,
      Len is Width >> 3,
      length(Octets, Len)
    },
    Octets.
