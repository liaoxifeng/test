%%%-------------------------------------------------------------------
%%% @author feng.liao
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 接收文件的变动，并且动态编译
%%% @end
%%% Created : 09. 三月 2019 下午7:46
%%%-------------------------------------------------------------------

-module(monitor).
-author("feng.liao").

-behaviour(gen_server).

-include("test.hrl").

%% API
-export([start_link/1,start/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%-define(path, "/mnt/d/work/zone/moli_local_1/lager_log/debug.log").
-define(path, "/mnt/d/work/zone/moli_local_1/log/log_join-log_1.txt").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    case lists:keymember(fs, 1, application:loaded_applications()) of
        false ->
            ignore;
        true ->
            start_link([default_fs])
    end.

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name]) ->
    fs:subscribe(Name),
    ?info("monitor start"),
    {ok, #{hang => hang()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({_Pid, {fs, file_event}, {?path, Flags}}, #{hang := Hang} = State) ->
    ?info("~p", [Flags]),
    case Flags of
        [modified] ->
            N = integer_to_list(hang() - Hang),
            String = os:cmd(lists:concat(["tail -", N, " ", ?path])),
            webhook:send_text(String, [], []),
            {noreply, State#{hang := hang()}};
        [created] ->
            {noreply, State#{hang := 0}};
        _ ->
            {noreply, State}
    end;
handle_info({_Pid, {fs, file_event}, {_Path, _Flags}}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

hang() ->
    [H | _] = string:tokens(os:cmd("wc -l " ++ ?path), " "),
    list_to_integer(H).