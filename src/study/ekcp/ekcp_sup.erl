%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十一月 2021 15:45
%%%-------------------------------------------------------------------
-module(ekcp_sup).
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
    AChild = [
        {ekcp_service_sup, {ekcp_service_sup, start_link, []}, permanent, 5000, supervisor, [ekcp_service_sup]}
        ,{ekcp_session_sup, {ekcp_session_sup, start_link, []}, permanent, 5000, supervisor, [ekcp_session_sup]}
        ,{ekcp_listen, {ekcp_listen, start_link, []}, permanent, 5000, worker, [ekcp_listen]}
    ],
    {ok, {{one_for_one, 5, 10}, AChild}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
