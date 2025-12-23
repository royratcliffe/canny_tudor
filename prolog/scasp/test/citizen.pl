eligible(Who) :- citizen(Who), not criminal(Who).

citizen(alice).
criminal(bob).

?- eligible(Who).
