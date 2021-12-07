%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 哈希-并查集
%%% p721:p().
%%% @end
%%% Created : 02. 十二月 2021 14:53
%%%-------------------------------------------------------------------
-module(p721).
-author("liaoxifeng").

%% API
-export([p/0]).

p() ->
    Accounts = [["David","David0@m.co","David1@m.co"],["David","David3@m.co","David4@m.co"],["David","David4@m.co","David5@m.co"],["David","David2@m.co","David3@m.co"],["David","David1@m.co","David2@m.co"]],
%%    Accounts = [
%%        ["John", "johnsmith@mail.com", "john00@mail.com"],
%%        ["John", "johnnybravo@mail.com"],
%%        ["John", "johnsmith@mail.com", "john_newyork@mail.com"],
%%        ["Mary", "mary@mail.com"]
%%    ],
    Len = length(Accounts),
    L = lists:seq(1, Len),
    Map = maps:from_list([{I, I} || I <- L]),
    Path = path(L, p(Accounts, 1, Map), #{}),
    format(maps:values(Path), Accounts, []).

p([], _N, Map) -> Map;
p([[Name|Mails]|Accounts], N, Map) ->
    Map2 = p2(Mails, Name, N, Map),
    p(Accounts, N + 1 , Map2).

p2([], _Name, _N, Map) -> Map;
p2([Mail|Mails], Name, N, Map) ->
    case maps:get(Mail, Map, undefined) of
        undefined -> p2(Mails, Name, N, Map#{Mail => N});
        Val -> p2(Mails, Name, N, union(Val, N, Map))
    end.

union(Val, N, Map) ->
    {Y, Map2} = find(N, Map),
    {X, Map3} = find(Val, Map2),
    Map3#{X => Y}.

find(X, Map) ->
    case maps:get(X, Map, undefined) of
        X -> {X, Map};
        Y ->
            {Z, Map2} = find(Y, Map),
            {Z, Map2#{X := Z}}
    end.

path([], _Map, Path) -> Path;
path([Node|L], Map, Path) ->
    {Head, _} = find(Node, Map),
    Val = maps:get(Head, Path, []),
    Path2 = maps:put(Head, [Node | Val], Path),
    path(L, Map, Path2).

format([], _Accounts, L) -> L;
format([H|Path], Accounts, L) ->
    [Name|_] = lists:nth(hd(H), Accounts),
    Val = format(H, Name, Accounts, []),
    format(Path, Accounts, [Val | L]).

format([], Name, _Accounts, L) -> [Name | lists:usort(L)];
format([H|Nodes], Name, Accounts, L) ->
    [_|Mails] = lists:nth(H, Accounts),
    format(Nodes, Name, Accounts, Mails ++ L).