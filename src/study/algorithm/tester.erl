%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 测试模块 执行 tester:test().
%%% @end
%%% Created : 20. 十月 2021 14:24
%%%-------------------------------------------------------------------
-module(tester).
-author("liaoxifeng").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([p/0]).

length_test() ->
    ?assert(length([1,2,3]) =:= 3).

fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

fib_test_() ->
    [?_assert(fib(0) =:= 1),
        ?_assert(fib(1) =:= 1),
        ?_assert(fib(2) =:= 2),
        ?_assert(fib(3) =:= 3),
        ?_assert(fib(4) =:= 5),
        ?_assert(fib(5) =:= 8),
        ?_assertException(error, function_clause, fib(-1)),
        ?_assert(fib(31) =:= 2178309)
    ].

basic_test_() ->
    fun () -> ?assert(1 + 1 =:= 2) end.


%%%===================================================================
%%% algorithm
%%%===================================================================
p66_test() ->
    L = [1,2,3],
    [H | L2] = lists:reverse(L),
    do_p66([H + 1 | L2], []).

do_p66([], Req) -> Req;
do_p66([H | L], Req) when H >= 10 ->
    R = H rem 10,
    P = H div 10,
    do_p66(to_next(P, L), [R | Req]);
do_p66([H | L], Req) ->
    do_p66(L, [ H| Req]).


to_next(P, []) -> [P];
to_next(P, [H|L]) -> [H + P | L].


p319_test() ->
    erlang:trunc(math:sqrt(10)).

p() ->
    ok.