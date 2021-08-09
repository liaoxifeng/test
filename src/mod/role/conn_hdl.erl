%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 五月 2021 11:32
%%%-------------------------------------------------------------------
-module(conn_hdl).
-author("liaoxifeng").

-include("role.hrl").
-include("common.hrl").


%% API
-export([do/2]).

do(#role{}, Msg) ->
    ?error("协议匹配错误 ~w", [Msg]),
    ok.