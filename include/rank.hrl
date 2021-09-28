%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 排行榜
%%% @end
%%% Created : 22. 九月 2021 16:07
%%%-------------------------------------------------------------------

-define(rank_hero_fc, 1).        %% 英雄战力
-define(rank_role_lev, 2).       %% 玩家等级


%% 5分钟刷新一次排行榜
-define(rank_sort_interval, 30000). %% 排行榜刷新时间

%% 所有列表
-define(rank_list_all, [?rank_hero_fc]).

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