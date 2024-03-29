%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 学习
%%% @end
%%% Created : 10. 八月 2021 9:50
%%%-------------------------------------------------------------------
-module(study_sup).
-author("liaoxifeng").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    code:load_file(tester),
    AChild = [
%%        {tcp_srv, {tcp_srv, start_link, []}, transient, 100000, worker, [tcp_srv]}
%%        ,{tcp_cli, {tcp_cli, start_link, []}, transient, 100000, worker, [tcp_cli]}
%%        ,{udp_srv, {udp_srv, start_link, []}, transient, 100000, worker, [udp_srv]}
%%        ,{udp_cli, {udp_cli, start_link, []}, transient, 100000, worker, [udp_cli]}
         {ekcp_sup, {ekcp_sup, start_link, []}, permanent, 5000, supervisor, [ekcp_sup]}
    ],
    {ok, {{one_for_one, 50, 1}, AChild}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
