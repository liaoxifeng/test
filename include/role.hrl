%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 玩家头文件
%%% @end
%%% Created : 30. 三月 2021 17:17
%%%-------------------------------------------------------------------
-author("liaoxifeng").

%% 玩家
-record(role, {
    id = 0             %% 玩家id
    ,name = ""         %% 玩家名称
    ,sex = 0           %% 性别 0男 1女
    ,sid = undefined   %% socket pid
    ,friends = []      %% 好友列表
}).

%% 玩家简略信息
-record(role_brief, {
    id = 0             %% 玩家id
    ,name = ""         %% 玩家名称
    ,sex = 0           %% 性别 0男 1女
}).
