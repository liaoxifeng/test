%%%-------------------------------------------------------------------
%%% @author luoxueqing
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 排行榜
%%% @end
%%% Created : 06. 十一月 2019 16:43
%%%-------------------------------------------------------------------
-author("luoxueqing").

-define(rank_hero_fc, 1).
-define(rank_hero_top5_fc, 2).
-define(rank_hero_lev, 3).
-define(rank_role_lev, 5).
-define(rank_tower,6).    								%% 试练塔总排行榜
-define(rank_tower_zone(Zone), {7, Zone}). 				%% 试练塔区域排行
-define(rank_tower_floor(Floor), {8, Floor}).			%% 试练塔层排行
%%-define(rank_weekly_dun(Weekly), {9, Weekly}). 		%% 周常（7大试炼）排行榜
-define(rank_weekly_dun, 9). 			                %% 周常（7大试炼）排行榜
%%-define(rank_weekly_dun_super, 10).         			%% 周本先驱榜（超级榜）
-define(rank_tower_season, 11).             			%% 封锁区赛季榜
-define(rank_endless, 12).      						%% 无尽试炼排行榜
%% 养成排行榜
-define(rank_role_fc, 13).								%% 玩家战力排行榜 (总战力)
-define(rank_hero_soldier, 14).                         %% 战士培养排行榜
-define(rank_hero_guard, 15).                           %% 守卫培养排行榜
-define(rank_hero_master, 16).                          %% 射手培养排行榜
-define(rank_hero_shooter, 17).                         %% 法师培养排行榜
-define(rank_hero_assist, 18).                          %% 辅助培养排行榜
-define(rank_old_treasure, 19).                         %% 古宝养成排行榜
-define(rank_guild, 20).                                %% 联盟排行榜
-define(rank_mission, 21).                              %% 主线进度排行榜
-define(rank_nurture_tower, 22).                        %% 封锁区养成排行榜
-define(rank_nurture_endless, 23).                      %% 无尽回廊养成排行榜
-define(rank_nurture_weekly_dun, 24).                   %% 哨所演练养成排行榜

-define(rank_dorm_room, 25).							%% 宿舍房间排行榜

%% 5分钟刷新一次排行榜
-define(rank_sort_interval, 30000). %% 排行榜刷新时间
-define(rank_clear_day, 15).        %% 排行榜清理时间，最后登录时间

%% 所有列表
-define(rank_list_all, [?rank_hero_fc, ?rank_hero_lev, ?rank_hero_top5_fc, ?rank_tower, ?rank_weekly_dun, ?rank_tower_season,
	?rank_endless, ?rank_role_fc, ?rank_hero_soldier, ?rank_hero_guard, ?rank_hero_master,
	?rank_hero_shooter, ?rank_hero_assist, ?rank_old_treasure, ?rank_guild, ?rank_mission, ?rank_nurture_tower,
	?rank_nurture_endless, ?rank_nurture_weekly_dun, ?rank_dorm_room]).

%% 养成排行榜
-define(rank_nurtures, [?rank_role_fc, ?rank_hero_soldier, ?rank_hero_guard, ?rank_hero_master, ?rank_hero_shooter,
	?rank_hero_assist, ?rank_old_treasure, ?rank_guild, ?rank_mission, ?rank_nurture_tower, ?rank_nurture_endless,
	?rank_nurture_weekly_dun]).

%% 复合主键榜类型{基础类型，值长度}
-define(rank_tower_floor_other, {8, 2}).
-define(rank_tower_zone_other,  {7, 2}).

%% 所有复合主键排行榜
-define(rank_list_all_other, [?rank_tower_floor_other, ?rank_tower_zone_other]).

-record(rank,{
	%% 排行榜类型
	rank_type = 0
	%% 榜单列表
	,list = []


}).


-define(rank_data_ver, 1).
%% 通用排行榜数据结构
-record(rank_data,{
	ver = ?rank_data_ver
	%% 排行榜类型
	,rank_type = 0
	%% 主键
	,key
	%% 玩家id
	,rid = {0, "", 0}
	%% 主值
	,val = 0
	%% 数据更新时间
	,time = 0
	%% 排名
	,index = 0
	%% 其他1
	,extra1
	%% 其他
	,extra2
	%% 其他3
	,extra3
	%% 是否脏写
	,dirty = false
}).

%% 排行榜配置
-record(rank_cfg,{
	%% 排序规则   [{KeyPos， asc | desc}]
	sort_rule = []
	%% 上榜条件 [{KeyPos, MinVal}]
	,require = []
	%% 长度
	,len = 100
	%% 刷新规则 common | realtime
	,refresh
	%% 排行榜名字
	,name = <<>>
}).

-define(to_name(Type),
	case Type of
		?rank_role_fc            ->	"玩家总战力排行榜";
		?rank_hero_soldier       -> "战士培养排行榜";
		?rank_hero_guard         -> "守卫培养排行榜";
		?rank_hero_master        -> "射手培养排行榜";
		?rank_hero_shooter       -> "法师培养排行榜";
		?rank_hero_assist        -> "辅助培养排行榜";
		?rank_old_treasure       -> "古宝养成排行榜";
		?rank_guild              -> "联盟排行榜";
		?rank_mission            -> "主线进度排行榜";
		?rank_nurture_tower      -> "封锁区养成排行榜";
		?rank_nurture_endless    -> "无尽回廊养成排行榜";
		?rank_nurture_weekly_dun -> "哨所演练养成排行榜";
		_ -> ""
	end
).

-define(rank_nurture_index_ver, 1).
%% 养成排行榜index
-record(rank_nurture_index,{
	ver = ?rank_nurture_index_ver,
	key = 0,                    %% 排行榜类型
	index = 0,                  %% index值 即target
	dirty = false               %% 是否脏写
}).

-define(rank_nurture_data_ver, 1).
%% 养成排行榜数据结构
-record(rank_nurture_data,{
	ver = ?rank_nurture_data_ver,
	rank_type = 0,              %% 排行榜类型
	key,                        %% 主键 {rank_type, target}
	target = 0,                 %% 目标
	rid = {0, "", 0},           %% 玩家id
	lev = 0,                    %% 玩家等级
	face = 0,                   %% 玩家头像
	name = "",                  %% 玩家名字
	head_frame = 1,             %% 头像框
	time = 0,                   %% 时间戳
	dirty = false,              %% 是否脏写
	status = 0                  %% 领奖状态
}).
