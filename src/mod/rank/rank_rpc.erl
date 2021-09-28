%%%-------------------------------------------------------------------
%%% @author luoxueqing
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 排行榜协议请求
%%% @end
%%% Created : 06. 十一月 2019 16:31
%%%-------------------------------------------------------------------
-module(rank_rpc).
-author("luoxueqing").
-include("common.hrl").

%% API
-export([
    handle/3
]).

%% 排行榜请求
handle(16300, {}, Role) ->
    role_nurture:push_16300(Role),
    ok;

%% 单个排行榜请求
handle(16301, {RankType}, Role) ->
    role_nurture:push_16301(Role, RankType),
    ok;

%% 排行榜点赞
handle(16302, {RankType, Id, Platform, ZoneId}, Role) ->
    case role_nurture:liked(Role, RankType, {Id, Platform, ZoneId}) of
        {ok, NRole} ->
            {reply, {?true, ""}, NRole};
        {false, Reason} ->
            {reply, {?false, Reason}}
    end;

%% 排行榜领取奖励
handle(16303, {RankType, Target}, Role) ->
    case role_nurture:get_reward(Role, RankType, Target) of
        {ok, NRole} ->
            {reply, {?true, ""}, NRole};
        {false, Reason} ->
            {reply, {?false, Reason}}
    end;

%% 封锁区排行榜奖励推送
handle(16304, {}, Role) ->
    role_nurture:push_16304(Role),
    ok;

%% 排行榜红点推送
handle(16305, {}, Role) ->
    role_nurture:push_16305(Role),
    ok;

handle(_Cmd, _Data, _role) ->
    ?ERR("模块[~w]收到无效的RPC调用[~w]: ~w", [?MODULE, _Cmd, _Data]),
    {error, unknow_command}.
