%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 排行榜协议请求
%%% @end
%%% Created : 22. 九月 2021 16:07
%%%-------------------------------------------------------------------%%%-------------------------------------------------------------------
-module(rank_rpc).
-include("common.hrl").

%% API
-export([
    handle/3
]).

%% 排行榜请求
handle(16300, {}, _Role) ->
    ok;

handle(_Cmd, _Data, _role) ->
    ?error("模块[~w]收到无效的RPC调用[~w]: ~w", [?MODULE, _Cmd, _Data]),
    {error, unknow_command}.
