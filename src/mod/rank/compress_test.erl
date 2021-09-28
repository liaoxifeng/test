%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 测试
%%% compress_test:init().
%%% ets:tab2list(rank_data).
%%% [{rank_data,1,1,{1,{1,[108,111,99,97,108],1}},{1,[108,111,99,97,108],1},1,1632745214,0,true},
%%% {rank_data,1,1,{1,{3,[108,111,99,97,108],1}},{3,[108,111,99,97,108],1},3,1632745214,0,true},
%%% {rank_data,1,1,{1,{2,[108,111,99,97,108],1}},{2,[108,111,99,97,108],1},2,1632745214,0,true}]
%%% @end
%%% Created : 23. 九月 2021 9:34
%%%-------------------------------------------------------------------
-module(compress_test).
-author("liaoxifeng").
-include("rank.hrl").
-include("common.hrl").

-define(max, 3).

%% API
-export([init/0, get/1, set/2]).

init() ->
    ets:delete_all_objects(rank_data),
    init(1).
init(N) -> init(N, []).
init(N, L) when N > ?max ->
    ets:insert(rank_data, L),
    rank:sort(?rank_list_all),
    ok;
init(N, L) ->
    RankType = util:rand_list(?rank_list_all),
    Rid = {N, "local", 1},
    H = #rank_data{
        rank_type = RankType,
        key = {RankType, Rid},
        rid = Rid,
        val = N,
        time = util:unixtime(),
        dirty = true
    },
    init(N + 1, [H | L]).

get(Id) ->
    Rid = {Id, "local", 1},
    statistics(runtime),
    statistics(wall_clock),
    Result = rank_api:get_role_rank(Rid),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    ?info("查询用时  ~w ms ~w us ", [Time1, Time2]),
    Result.

set(Id, Val) ->
    Rid = {Id, "local", 1},
    RankData = #rank_data{
        rank_type = ?rank_hero_fc,
        key = {?rank_hero_fc, Rid},
        rid = Rid,
        val = Val,
        time = util:unixtime(),
        dirty = true
    },
    statistics(runtime),
    statistics(wall_clock),
    rank_api:update_rank_data(RankData),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    ?info("插入用时 ~w ms ~w us", [Time1, Time2]),
    ok.

