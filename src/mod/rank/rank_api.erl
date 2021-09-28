%%%-------------------------------------------------------------------
%%% @author luoxueqing
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 排行榜接口
%%% @end
%%% Created : 06. 十一月 2019 17:02
%%%-------------------------------------------------------------------
-module(rank_api).
-author("luoxueqing").
-include("rank.hrl").
-include("role.hrl").
-include("common.hrl").
-include("dungeon.hrl").

%% API
-export([
	convert/2
	,get_rank_data/2
	,get_role_rank/1
	,off_rank/1
	,update_rank_data/1
	,get_rank_list/1
	,get_rank_nurture_data/1
	,get_rank_nurture_data/2
    ,del_rank_data/2
]).

%% 获取排行榜榜单列表
get_rank_list(RankType) ->
	case ets:lookup(rank, RankType) of
		[#rank{list = RankList}] ->
			case lists:member(RankType, ?rank_nurtures) of
				true -> lists:sublist(RankList, 10);
				_ -> RankList
			end;
		_ -> []
	end.

%% 获取玩家某个排行榜数据
get_rank_data(RankType, RoleId) ->
	case ets:lookup(rank_data, {RankType, RoleId}) of
		[#rank_data{} = Rank] ->
			Rank;
		_ -> false
	end.

%% 获取某个养成排行榜首次达到目标数据
get_rank_nurture_data(RankType) ->
	case ets:match_object(rank_nurture_data, #rank_nurture_data{rank_type = RankType, _ = '_'}) of
		RankData when is_list(RankData) -> RankData;
		_ -> []
	end.

%% 获取某个养成排行榜某个目标首次达到目标数据
get_rank_nurture_data(RankType, Target) ->
	case ets:lookup(rank_nurture_data, {RankType, Target}) of
		[#rank_nurture_data{} = Rank] -> Rank;
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
	RankData = RankData0#rank_data{time = date:unixtime()},
	case check_min(Require, RankData) of
		true ->
			case ets:lookup(rank_data, {RankType, RoleId}) of
				[OldRankData] ->
					case rank_val(RankData, OldRankData, Rule) of
						false -> ok;
						true ->
							RankData2 = RankData#rank_data{key = {RankType, RoleId}, dirty = true},
							RankData3 = ?IF(lists:member(RankType, ?rank_nurtures), RankData2#rank_data{extra1 = OldRankData#rank_data.extra1}, RankData2),
							ets:insert(rank_data, RankData3),
							update_rank_nurture(RankData3),
							fresh_rank(RankType, FreshType)
					end;
				_ ->
					RankData2 = RankData#rank_data{key = {RankType, RoleId}, extra1 = 0, dirty = true},
					ets:insert(rank_data, RankData2),
					update_rank_nurture(RankData2),
					fresh_rank(RankType, FreshType)
			end;
		false -> ok
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

%% 计算排行榜最新的值
%%rank_val(Old, New, Rule) ->
%%	case lists:keyfind(#rank_data.val, 1, Rule) of
%%		{_, desc} -> max(Old, New);
%%		{_, asc} -> min(Old, New)
%%	end.

fresh_rank(Type, realtime) -> rank ! {sort, Type};
fresh_rank(_, _) -> skip.

%% 更新目标完成
update_rank_nurture(#rank_data{rid = Rid, rank_type = RankType, val = Val, extra2 = {Lev, Face, HeadFrame, Name}}) ->
	L = [N || N <- rank_data:get_target(RankType), Val >= N],
	Hit = case ets:lookup(rank_nurture_index, RankType) of
			  [#rank_nurture_index{index = Index}] -> [N || N <- L, N > Index];
			  _ -> L
		  end,
	case Hit of
		[] -> ok;
		_ ->
			F = fun(I) ->
				#rank_nurture_data{
					rank_type = RankType,
					key = {RankType, I},
					target = I,
					rid = Rid,
					lev = Lev,
					face = Face,
					head_frame = HeadFrame,
					name = Name,
					time = date:unixtime(),
					dirty = true
				}
				end,
			Data = lists:map(F, Hit),
			ets:insert(rank_nurture_data, Data),
			RankIndex = #rank_nurture_index{key = RankType, index = lists:max(Hit), dirty = true},
			ets:insert(rank_nurture_index, RankIndex),
			role_online_mgr:apply({role_nurture, push_prompt, [RankType]}),
			ok
	end;
update_rank_nurture(_) -> ok.


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

%% 排行榜数据转换
convert(?rank_role_lev, #role{lev = Lev, id = RoleId}) ->
	#rank_data{rank_type = ?rank_role_lev, key = {?rank_role_lev, RoleId}, val = Lev};
convert(?rank_hero_top5_fc, #role{id = RoleId}) ->
	#rank_data{rank_type = ?rank_hero_top5_fc, key = {?rank_hero_top5_fc, RoleId}};
convert(?rank_hero_lev, #role{id = RoleId}) ->
	#rank_data{rank_type = ?rank_hero_lev, key = {?rank_hero_lev, RoleId}};
%%convert(?rank_tower_floor(Floor), #role{id = RoleId, dun_tower = #dun_tower{max_suc_num = MSN}}) ->
%%	#rank_data{rank_type = ?rank_tower_floor, key = {?rank_tower_floor, RoleId}, val = MSN};
convert(Type, _) -> ?ERR("~n排行榜数据转换出现错误，找不到排行榜类型[~w]", [Type]), false.

