:- begin_tests(canny_redis).
:- use_module(redis).

test(redis_stream_id, [fail]) :-
    redis_stream_id(_, _).
test(redis_stream_id, [fail]) :-
    redis_stream_id(0-0, _).
test(redis_stream_id, [true(A=='0-0')]) :-
    redis_stream_id(A, 0-0).
test(redis_stream_id, [true(A=='1111-2222')]) :-
    redis_stream_id(A, 1111-2222).

:- end_tests(canny_redis).
