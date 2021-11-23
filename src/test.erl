%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 项目启动入口
%%% @end
%%% Created : 22. 十月 2020 11:09
%%%-------------------------------------------------------------------
-module(test).
-author("liaoxifeng").
-include("common.hrl").

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    start/0,
    stop/1,
    stop/0,
    stop_server/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
%% 启动项目
start() ->
    {ok, _} = application:ensure_all_started(lager),
    ok = ssl:start(),
    ok = inets:start(),
    ok = application:start(fs),
    {ok, _} = application:ensure_all_started(mongodb),
    monitor:start(),
    ok = application:start(test),
    ?info("projec start success"),
    ok.

%% 关闭项目
stop() ->
    io:setopts([{encoding, unicode}]),
    case init:get_plain_arguments() of
        [NodeStr | _] ->
            Node = list_to_atom(NodeStr),
            case rpc:call(Node, test, stop_server, [stop]) of
                ok ->
                    rpc:cast(Node, test, stop_server, [halt]),
                    io:format("~ts 已关闭~n", [Node]);
                _ ->
                    io:format("~ts 关闭失败~n", [Node])
            end;
        _ ->
            io:format("结点输入错误~n")
    end,
    halt().

%% 关闭服务器
stop_server(stop) ->
    ok = application:stop(test),
    ok;
stop_server(_) ->
    halt().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
        StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    Extra = init:get_plain_arguments(),
    ?info("Extra ~p", [Extra]),
    case zone:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
