:- begin_tests(prefix_sum).
:- use_module(sum).

test(sum, [A == [3, 2, 6, 8]]) :- prefix_sum([3, -1, 4, 2], A).
test(sum, [A == [1, 3, 6, 10]]) :- prefix_sum([1, 2, 3, 4], A).

test(range_sum, true(Sum =:= 5)) :-
    prefix_sum([3, -1, 4, 2], PrefixSum),
    % Sum of elements from index 1 to 3 (0-based): -1 + 4 + 2 = 5! Using the
    % prefix sum array, this can be computed as PrefixSum[3] - PrefixSum[1].
    range_sum(PrefixSum, 1, 3, Sum).

:- end_tests(prefix_sum).
