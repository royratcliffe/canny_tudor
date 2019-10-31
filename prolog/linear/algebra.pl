:- module(linear_algebra,
          [   matrix_dimensions/3,
              matrix_identity/2,        % +Order, -Matrix
              matrix_transpose/2,       % ?Matrix0, ?Matrix
              matrix_rotation/2,

              vector_dimension/2,
              vector_distance/2,        % ?V, ?Distance
              vector_distance/3,        % ?U, ?V, ?Distance
              vector_translate/3,
              vector_multiply/3,
              vector_scale/3,           % ?Scalar, ?U, ?V
              vector_heading/2,         % ?V, ?Heading

              scalar_translate/3,
              scalar_multiply/3,
              scalar_power/3            % ?X, ?Y, ?Z
          ]).

:- use_module(library(clpr)).

matrix_dimensions(Matrix, Rows, Columns) :-
    length(Matrix, Rows),
    matrix_dimensions_(Matrix, Columns).

matrix_dimensions_([], _).
matrix_dimensions_([V0|V], Columns) :-
    vector_dimension(V0, Columns),
    matrix_dimensions_(V, Columns).

%!  matrix_identity(+Order:nonneg, -Matrix:list(list(number))) is
%!  semidet.
%
%   Matrix becomes an identity matrix of Order dimensions. The result is
%   a square diagonal matrix of Order rows and Order columns.
%
%   The first list of scalars  (call  it   a  row  or  column) becomes 1
%   followed by Order-1 zeros.  Subsequent   scalar  elements  become an
%   Order-1 identity matrix with a 0-scalar   prefix for every sub-list.
%   Operates recursively albeit without tail recursion.
%
%   Fails when matrix size Order is less than zero.

matrix_identity(0, []) :-
    !.
matrix_identity(Order, [[1|Vector]|Matrix]) :-
    integer(Order),
    Order > 0,
    Order0 is Order - 1,
    vector_dimension(Vector, Order0),
    maplist(=(0), Vector),
    matrix_identity(Order0, Matrix0),
    maplist([Scalars, [0|Scalars]]>>true, Matrix0, Matrix).

%!  matrix_transpose(?Matrix0:list(list(number)),
%!  ?Matrix:list(list(number))) is semidet.
%
%   Transposes matrices. The matrix is a list of lists. Fails unless all
%   the sub-lists share the same length.   Works in both directions, and
%   works with non-numerical elements. Only  operates   at  the level of
%   two-dimensional lists, a list with   sub-lists. Sub-sub-lists remain
%   lists and un-transposed if sub-lists comprise list elements.

matrix_transpose([U0|U], []) :-
    maplist(=([]), [U0|U]),
    !.
matrix_transpose([U0|U], [V0|V]) :-
    maplist([[H|T], H, T]>>true, [U0|U], V0, U_),
    matrix_transpose(U_, V).

matrix_rotation(Theta, [[A, B], [C, A]]) :-
    {A =:= cos(Theta), B =:= sin(Theta), C =:= -B}.

vector_dimension(V, Columns) :- length(V, Columns).

%!  vector_distance(?V:list(number), ?Distance:number) is semidet.
%!  vector_distance(?U:list(number), ?V:list(number), ?Distance:number)
%!  is semidet.
%
%   Distance of the vector V  from   its  origin.  Distance is Euclidean
%   distance between two vectors where the   first vector is the origin.
%   Note that Euclidean  is  just  one   of  many  distances,  including
%   Manhattan and chessboard, etc.  The   predicate  is called distance,
%   rather than length. The term length overloads  on the dimension of a
%   vector, its number of numeric elements.

vector_distance(V, Distance) :-
    maplist(scalar_power(2), V, [X0|X]),
    foldl(scalar_translate, X, X0, Scalar),
    scalar_power(2, Distance, Scalar),
    !.

vector_distance(U, V, Distance) :-
    vector_translate(U, W, V),
    vector_distance(W, Distance).

vector_translate([], [], []).
vector_translate([X|U], [Y|V], [Z|W]) :-
    scalar_translate(X, Y, Z),
    vector_translate(U, V, W).

vector_multiply([], [], []).
vector_multiply([X|U], [Y|V], [Z|W]) :-
    scalar_multiply(X, Y, Z),
    vector_multiply(U, V, W).

%!  vector_scale(?Scalar:number, ?U:list(number), ?V:list(number)) is
%!  nondet.
%
%   Vector U scales by Scalar to V.
%
%   What is the difference between   multiply  and scale? Multiplication
%   multiplies two vectors whereas scaling  multiplies   a  vector  by a
%   scalar; hence the verb to scale. Why is   the scalar at the front of
%   the argument list? This allows the meta-call of vector_scale(Scalar)
%   passing two vector arguments, e.g. when mapping lists of vectors.
%
%   The implementation performs non-deterministically because the CLP(R)
%   library  leaves  a  choice  point  when  searching  for  alternative
%   arithmetical solutions.

vector_scale(_, [], []).
vector_scale(Scalar, [X|U], [Y|V]) :-
    scalar_multiply(Scalar, X, Y),
    vector_scale(Scalar, U, V).

%!  vector_heading(?V:list(number), ?Heading:number) is semidet.
%
%   Heading in radians of vector V.   Succeeds  only for two-dimensional
%   vectors. Normalises the Heading  angle  in   (+,  -)  mode; negative
%   angles  wrap  to  the  range  between   pi  and  two-pi.  Similarly,
%   normalises the vector V in (-, +) mode; V has unit length.

vector_heading([X, Y], Heading) :-
    var(Heading),
    !,
    Angle is atan2(Y, X),
    (   Angle < 0
    ->  Heading is Angle + 2 * pi
    ;   Heading = Angle
    ).
vector_heading([X, Y], Heading) :-
    Y is sin(Heading),
    X is cos(Heading).

scalar_translate(X, Y, Z) :- {Z =:= X + Y}.

scalar_multiply(X, Y, Z) :- {Z =:= X * Y}.

%!  scalar_power(?X:number, ?Y:number, ?Z:number) is nondet.
%
%   Z is Y to the power X.
%
%   The first argument X is the  exponent   rather  than Y, first rather
%   than second argument. This allows  you   to  curry  the predicate by
%   fixing the first exponent argument.  In other words, scalar_power(2,
%   A, B) squares A to B.

scalar_power(X, Y, Z) :- {Z =:= pow(Y, X)}.
