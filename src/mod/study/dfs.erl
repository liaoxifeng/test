%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 最大岛屿问题 todo map存储数组  #{{X,Y} => {Island, Flag}}
%%% @end
%%% Created : 20. 十月 2021 14:38
%%%-------------------------------------------------------------------
-module(dfs).
-author("liaoxifeng").

%% API
-export([t/0]).

t() ->
    List = [[0,0,1,0,0,0,0,1,0,0,0,0,0],[0,0,0,0,0,0,0,1,1,1,0,0,0],[0,1,1,0,1,0,0,0,0,0,0,0,0],[0,1,0,0,1,1,0,0,1,0,1,0,0],[0,1,0,0,1,1,0,0,1,1,1,0,0],[0,0,0,0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,1,1,1,0,0,0],[0,0,0,0,0,0,0,1,1,0,0,0,0]],
    M = length(List),
    N = length(hd(List)),
    case M >= 1 andalso M =< 50 andalso N >= 1 andalso N =< 50 of
        true ->
            List2 = convert(lists:flatten(List), N),
            do(1, M, N, list_to_tuple(List2), 0);
        _ ->
            error
    end.

to_x(N, Len) ->
    case N rem Len of
        0 -> Len;
        X0 -> X0
    end.

to_y(N, Len) ->
    case ceil(N div Len) of
        Y0 when N rem Len =:= 0 -> Y0 - 1;
        Y0 -> Y0
    end.

convert(L, Len) -> convert(L, Len, [], 0).
convert([], _Len, Req, _N) -> lists:reverse(Req);
convert([H | L], Len, Req, N) ->
    N2 = N + 1,
    Y = to_y(N2, Len),
    X = to_x(N2, Len),
    convert(L, Len, [{{N2, X, Y}, H, 0} | Req], N2).

do(P, M, N, _T, Max) when P > M * N -> Max;
do(P, M, N, T, Max) ->
    case element(P, T) of
        {_, 0, _} ->
            do(P + 1, M, N, T, Max);
        {_, 1, 1} ->
            do(P + 1, M, N, T, Max);
        Elem ->
            {Len, T2} = do2(Elem, N, M * N, T),
            do(P + 1, M, N, T2, max(Len, Max))
    end.

%% 节点处理
do2({{P, X, Y}, 0, _}, _N, _ELen,  T) -> {0, setelement(P, T, {{P, X, Y}, 0, 1})};
do2({_, _, 1}, _N, _ELen, T) -> {0, T};
do2({{P, X, Y}, 1, _}, N, ELen, T0) ->
    T = setelement(P, T0, {{P, X, Y}, 1, 1}),
    %% 向左
    P2 = P - 1,
    {Len2, T2} =
        case to_y(P2, N) =:= Y andalso P2 > 0 of
            false -> {0, T};
            _ ->
                Elem = element(P2, T),
                do2(Elem, N, ELen, T)
        end,

    %% 向右
    P3 = P + 1,
    {Len3, T3} =
        case to_y(P3, N) =:= Y andalso P3 =< ELen of
            false -> {0, T2};
            _ ->
                Elem3 = element(P3, T2),
                do2(Elem3, N, ELen, T2)
        end,

    %% 向下
    P4 = (Y + 1) * N + X,
    {Len4, T4} =
        case P4 =< ELen of
            false -> {0, T3};
            _ ->
                Elem4 = element(P4, T3),
                do2(Elem4, N, ELen, T3)
        end,

    %% 向上
    P5 = (Y - 1) * N + X,
    {Len5, T5} =
        case P5 > 0 of
            false -> {0, T4};
            _ ->
                Elem5 = element(P5, T4),
                do2(Elem5, N, ELen, T4)
        end,
    {1 + Len2 + Len3 + Len4 + Len5, T5}.