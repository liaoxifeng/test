%%%-------------------------------------------------------------------
%%% @author luoxueqing
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 排行榜配置
%%% @end
%%% Created : 06. 十一月 2019 16:31
%%%-------------------------------------------------------------------
-module(rank_cfg).
-author("luoxueqing").
-include("rank.hrl").

%% API
-export([
	get/1
]).

get(?rank_hero_top5_fc) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 50000}]
		,len = 100
		,refresh = common
	};
get(?rank_role_lev) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 50000}]
		,len = 100
		,refresh = realtime
		,name = "玩家等级"
	};
get(?rank_hero_lev) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 50000}]
		,len = 100
		,refresh = realtime
		,name = "英雄等级"
	};
get(?rank_hero_fc) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 50000}]
		,len = 100
		,refresh = realtime
		,name = "英雄战力"
	};
get(?rank_tower) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 9999999
		,refresh = realtime
		,name = "试练塔（总）"
	};
get(?rank_tower_zone(_)) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 10
		,refresh = realtime
		,name = "试练塔（区）"
	};
get(?rank_tower_floor(_)) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 10
		,refresh = realtime
		,name = "试练塔（层）"
	};
get(?rank_weekly_dun) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 99999
		,refresh = realtime
		,name = "周常副本周目榜"
	};
get(?rank_tower_season) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.extra1, asc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 9999999
		,refresh = realtime
		,name = "封锁区赛季玩法排行榜"
	};
get(?rank_endless) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 9999999
		,refresh = realtime
		,name = "无尽试炼排行榜"
	};
get(?rank_role_fc) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "玩家战力排行榜"
	};
get(?rank_hero_soldier) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "战士培养排行榜"
	};
get(?rank_hero_guard) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "守卫培养排行榜"
	};
get(?rank_hero_master) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "射手培养排行榜"
	};
get(?rank_hero_shooter) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "法师培养排行榜"
	};
get(?rank_hero_assist) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "辅助培养排行榜"
	};
get(?rank_old_treasure) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "古宝养成排行榜"
	};
get(?rank_guild) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "联盟排行榜"
	};
get(?rank_mission) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "主线进度排行榜"
	};
get(?rank_nurture_tower) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "封锁区养成排行榜"
	};
get(?rank_nurture_endless) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "无尽回廊养成排行榜"
	};
get(?rank_nurture_weekly_dun) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}, {#rank_data.time, asc}]
		,require = [{#rank_data.val, 1}]
		,len = 100
		,refresh = realtime
		,name = "哨所演练养成排行榜"
	};
get(?rank_dorm_room) ->
	#rank_cfg{
		sort_rule = [{#rank_data.val, desc}]
		,require = [{#rank_data.val, 1}]
		,len = 50
		,refresh = realtime
		,name = "宿舍房间排行榜"
	};
get(_) -> false.