:- begin_tests(canny_permutations).

:- use_module(permutations).

:- public test/2.

test(permute_sum_of_int, [all(A==[[1, 1, 1], [1, 2], [2, 1], [3]])]) :-
    permute_sum_of_int(3, A).

test(permute_list_to_grid, [true(A==[[a]])]) :-
    permute_list_to_grid([a], A).
test(permute_list_to_grid, [all(A==[[[a], [b]], [[a, b]]])]) :-
    permute_list_to_grid([a, b], A).
test(permute_list_to_grid,
     [   all(A==[   [[a], [b], [c]],
                    [[a], [b,   c]],
                    [[a,   b], [c]],
                    [[a,   b,   c]]
                ])
     ]) :-
    permute_list_to_grid([a, b, c], A).

:- end_tests(canny_permutations).
