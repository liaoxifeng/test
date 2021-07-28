%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 三月 2021 19:57
%%%-------------------------------------------------------------------
-module(demo).
-author("liaoxifeng").

-include("test.hrl").

-behaviour(gen_fsm).

%% API
-export([start_link/0, cast/0, call/0, call2/0]).

%% gen_fsm callbacks
-export([
    init/1,
    open/2,
    open/3,
    close/2,
    close/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4
]).


%%%===================================================================
%%% API
%%%===================================================================

%% demo:call().      demo:cast().     demo:call2().
cast() ->
    gen_fsm:send_event(?MODULE, hello).

call() ->
    gen_fsm:sync_send_event(?MODULE, ooo).

call2() ->
    gen_fsm:sync_send_all_state_event(?MODULE, ooo).

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->
    {ok, open, #{}, 1000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------

open(_Event, State) ->
    ?info("open open open open "),
    {next_state, close, State, 5000}.

close(_Event, State) ->
    ?info("close close close close"),
    {next_state, open, State, 5000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------

open(_Event, _From, State) ->
    ?info("sync_send_event open"),
    {reply, ok, close, State, 5000}.

close(_Event, _From, State) ->
    ?info("sync_send_event close"),
    {reply, ok, open, State, 5000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------

handle_sync_event(_Event, From, StateName, State) ->
    ?info("sync_send_all_state_event ~w", [StateName]),
    StateName2 = case StateName of
                     open -> close;
                     _ -> open
                 end,
    gen_fsm:reply(From, ok),
    {next_state, StateName2, State, 5000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
