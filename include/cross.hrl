%%%-------------------------------------------------------------------
%%% @author luoxueqing
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 跨服头文件
%%% @end
%%% Created : 23. 三月 2020 17:47
%%%-------------------------------------------------------------------
-author("luoxueqing").



%% 节点镜像信息
-record(mirror, {
	%% {平台标识,core表示中立节点, 分区号}
	id = {"", 0}                      :: {bitstring(), non_neg_integer()}
	%% 进程pid
	,pid
	%% 节点名称
	,nodename                           :: atom()
	%% magic cookie
	,cookie = "m8B2E89I@O1n3z1"         :: string()
	%% 版本
	,ver = ""                           :: string()
	%% 合服列表
	,combine = []
}
).

%% 监控器
-record(monitor,{
	%% 被监控的mirror主键
	id = {"", 0}
	%% 被监控的节点
	,node   :: atom()
	%% 被监控的pid
	,pid    :: pid()
	%% 被监控的mod
	,mod    :: atom()
	%% 监控引用
	,ref    :: pid()
	%% 监控器父进程pid
	,master :: pid()
}).

%% 合服映射
-record(merge_mapping,{
	zone = {"", 0}
	,to = {"", 0}
}).