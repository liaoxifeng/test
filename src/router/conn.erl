%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十月 2020 11:10
%%%-------------------------------------------------------------------
-module(conn).
-author("liaoxifeng").

-behaviour(gen_server).

%% API
-export([create/4]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(conn, {
    type = 0,
    socket = 0,
    ip = 0,
    port = 0,
    loop_counter = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

create(ClientType, Socket, Ip, Port) ->
    gen_server:start(?MODULE, [ClientType, Socket, Ip, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([ClientType, Socket, Ip, Port]) ->
    process_flag(trap_exit, true),
    erlang:send_after(10000, self(), loop),
    State = #conn{type = ClientType, socket = Socket, ip = Ip, port = Port},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(Request, _From, State) ->
    io:format("~p ~p ~w~n", [?MODULE, ?LINE, Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast(Request, State) ->
    io:format("~p ~p ~w~n", [?MODULE, ?LINE, Request]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(loop, State = #conn{loop_counter = C}) ->
    case C rem 18 =:= 0 of
        false -> ignore;
        true -> garbage_collect()
    end,
    erlang:send_after(10000, self(), loop),
    {noreply, State#conn{loop_counter = C + 1}};

handle_info(Info, State) ->
    io:format("~p ~p ~w~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
