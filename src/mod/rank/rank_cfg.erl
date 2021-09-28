%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 排行榜配置
%%% @end
%%% Created : 22. 九月 2021 16:07
%%%-------------------------------------------------------------------
-module(rank_cfg).
-author("luoxueqing").
-include("rank.hrl").

%% API
-export([
	get/1
]).

get(?rank_role_lev) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "玩家等级"
	};
get(?rank_hero_fc) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "英雄战力"
	};
get(_) -> false.