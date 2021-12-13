%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% p807:p().
%%% @end
%%% Created : 13. 十二月 2021 16:33
%%%-------------------------------------------------------------------
-module(p807).
-author("liaoxifeng").

%% API
-export([p/0]).

p() ->
    Grid = [[3,0,8,4],[2,4,5,7],[9,2,6,3],[0,3,1,0]],
    {Map, Len, Start} = low(Grid, #{}, 1, 0),
    L = lists:flatten(Grid),
    Map2 = col(Len, L, Map),
    End = cal({1, 1}, Len, Map2, 0),
    End - Start.

cal({Low, _Col}, Len, _Map, Sum) when Low > Len -> Sum;
cal({Low, Col}, Len, Map, Sum) when Col > Len ->
    cal({Low + 1, 1}, Len, Map, Sum);
cal({Low, Col}, Len, Map, Sum) ->
    LowMax = maps:get({low, Low}, Map),
    ColMax = maps:get({col, Col}, Map),
    cal({Low, Col + 1}, Len, Map, Sum + min(LowMax, ColMax)).

col(Len, L, Map) when Len > 0 ->
    {L2, Item} = col2(1, Len, L, [], []),
    Max = lists:max(Item),
    col(Len - 1, L2, Map#{{col, Len} => Max});
col(_, _L, Map)  -> Map.

col2(_N, 1, L, _NewL, _Item) -> {[], L};
col2(_N, _Len, [], NewL, Item) -> {lists:reverse(NewL), Item};
col2(N, Len, [H | L], NewL, Item) ->
    case N rem Len of
        0 -> col2(N + 1, Len, L, NewL, [H | Item]);
        _ -> col2(N + 1, Len, L, [H | NewL], Item)
    end.

low([], Map, N, All) -> {Map, N - 1, All};
low([L | Grid], Map, N, All) ->
    Sum = lists:sum(L),
    Max = lists:max(L),
    low(Grid, Map#{{low, N} => Max}, N + 1, All + Sum).