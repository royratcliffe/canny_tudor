:- module(linear_algebra,
          [   matrix_dimensions/3,
              matrix_rotation/2,

              vector_dimension/2,
              vector_translate/3,
              vector_multiply/3,
              vector_scale/3,

              scalar_translate/3,
              scalar_multiply/3,
              scalar_power/3
          ]).

:- use_module(library(clpr)).

matrix_dimensions(Matrix, Rows, Columns) :-
    length(Matrix, Rows),
    matrix_dimensions_(Matrix, Columns).

matrix_dimensions_([], _).
matrix_dimensions_([V0|V], Columns) :-
    vector_dimension(V0, Columns),
    matrix_dimensions_(V, Columns).

matrix_rotation(Theta, [[A, B], [C, A]]) :-
    {A =:= cos(Theta), B =:= sin(Theta), C =:= -B}.

vector_dimension(V, Columns) :- length(V, Columns).

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

scalar_translate(X, Y, Z) :- {Z =:= X + Y}.

scalar_multiply(X, Y, Z) :- {Z =:= X * Y}.

scalar_power(X, Y, Z) :- {Z =:= pow(Y, X)}.
