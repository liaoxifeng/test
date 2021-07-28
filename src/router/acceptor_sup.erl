%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 接收器
%%% @end
%%% Created : 23. 十月 2020 9:05
%%%-------------------------------------------------------------------
-module(acceptor_sup).
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

init([]) ->
    AChild = {acceptor, {acceptor, start_link, []}, transient, 2000, worker, [acceptor]},
    {ok, {{simple_one_for_one, 50, 1}, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


