%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 排行榜接口
%%% @end
%%% Created : 22. 九月 2021 16:07
%%%-------------------------------------------------------------------
-module(rank_api).
-author("luoxueqing").
-include("rank.hrl").
-include("role.hrl").
-include("common.hrl").

%% API
-export([
	convert/2
	,get_rank_data/2
	,get_role_rank/1
	,off_rank/1
	,update_rank_data/1
	,get_rank_list/1
    ,del_rank_data/2
]).

%% 获取排行榜榜单列表
get_rank_list(RankType) ->
	case ets:lookup(rank, RankType) of
		[#rank{list = RankList}] -> RankList;
		_ -> []
	end.

%% 获取玩家某个排行榜数据
get_rank_data(RankType, RoleId) ->
	case ets:lookup(rank_data, {RankType, RoleId}) of
		[#rank_data{} = Rank] ->
			Rank;
		_ -> false
	end.

%% 删除某个数据
del_rank_data(Table, Key) ->
    ets:delete(Table, Key),
    dets:delete(Table, Key).

%% @doc 更新排行榜数据，只有值发生变化才会真正写入
%% @spec update_rank_data(RankType, #role{}) -> ok
%% RankType :: integer() | lists()
update_rank_data(RankType, Role) when is_integer(RankType) ->
	update_rank_data(convert(RankType, Role));
update_rank_data([], _) -> ok;
update_rank_data([RankType | T], Role) ->
	update_rank_data(convert(RankType, Role)),
	update_rank_data(T, Role).

%% @doc 更新排行榜数据，只有值发生变化才会真正写入
%% @spec update_rank_data(Data) -> ok
%% Data :: #rank_data{} | #role{}
update_rank_data(false) -> ok;
update_rank_data(Role = #role{}) ->
	update_rank_data(?rank_list_all, Role);
update_rank_data(RankData0 = #rank_data{rid = RoleId, rank_type = RankType}) ->
	#rank_cfg{refresh = FreshType, sort_rule = Rule, require = Require} = rank_cfg:get(RankType),
	RankData = RankData0#rank_data{time = util:unixtime()},
	case check_min(Require, RankData) of
		true ->
			case ets:lookup(rank_data, {RankType, RoleId}) of
				[OldRankData] ->
					case rank_val(RankData, OldRankData, Rule) of
						false ->
							ok;
						true ->
							RankData2 = RankData#rank_data{key = {RankType, RoleId}, dirty = true},
							ets:insert(rank_data, RankData2),
							fresh_rank(RankType, FreshType)
					end;
				_ ->
					RankData2 = RankData#rank_data{key = {RankType, RoleId}, dirty = true},
					ets:insert(rank_data, RankData2),
					fresh_rank(RankType, FreshType)
			end;
		false ->
			ok
	end.

check_min([], _) -> true;
check_min([{KeyPos, MinVal} | T], RankData) ->
	?IF(element(KeyPos, RankData) >= MinVal, check_min(T, RankData), false).

%% 计算排行比较值是否可上榜
rank_val(_, _, []) -> false;
rank_val(RankData, OldRankData, [{Pos, Comp} | Rule]) ->
	Val = element(Pos, RankData),
	OldVal = element(Pos, OldRankData),
	case rank_val(OldVal, Val, Comp) of
		next -> rank_val(RankData, OldRankData, Rule);
		Boolean -> Boolean
	end;
rank_val(Val, Val, _) -> next;
rank_val(Old, New, desc) -> New > Old;
rank_val(Old, New, asc) -> New < Old.

fresh_rank(Type, realtime) -> rank ! {sort, Type};
fresh_rank(_, _) -> skip.

%% @doc 获取玩家所有排行榜数据
get_role_rank(RoleId) ->
	get_role_rank(?rank_list_all, RoleId, []).
get_role_rank([], _, RankRole) -> RankRole;
get_role_rank([RankType | T], RoleId, RankRole) ->
	case get_rank_data(RankType, RoleId) of
		false -> get_role_rank(T, RoleId, RankRole);
		Rank -> get_role_rank(T, RoleId, [Rank | RankRole])
	end.

%% 让某个玩家下榜
off_rank(RoleId) ->
	ets:match_delete(rank_data, #rank_data{rid = RoleId, _ = '_'}),
	rank ! sort_now.


convert(_Type, _) -> false.

