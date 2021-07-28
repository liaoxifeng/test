%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十月 2020 9:05
%%%-------------------------------------------------------------------
-module(client).
-author("liaoxifeng").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-export([
	connect/0
	,connect/2
	,close/0
]).

-define(local_host, "127.0.0.1").

-record(state, {
	socket
	,recv_pid
}).

%%%===================================================================
%%% API
%%%===================================================================

%% 请求连接
connect() ->
	connect("192.168.31.13", 8801).
connect(Host, Port) ->
	gen_server:cast(?MODULE, {connect, Host, Port}).

%% 断开连接
close() ->
	gen_server:cast(?MODULE, close).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

init([]) ->
	%% 心跳包
	erlang:send_after(10000, self(), ping),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% gen_tcp:send(S, <<"web_conn---------------">>).
%% @end
%%--------------------------------------------------------------------
%% gen_tcp:connect("192.168.31.13", 10000, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}]).
handle_cast({connect, Host, Port}, State) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}]),
	case gen_tcp:send(Socket, <<"game_client------------">>) of
		ok -> skip;
		{error, Reason} ->
			io:format("connect error :~w~n",[Reason])
	end,
	OwnerPid = self(),
	RecvPid = spawn(fun() -> loop_recv(Socket, OwnerPid) end),
	{noreply, State#state{socket = Socket, recv_pid = RecvPid}};
handle_cast({rpc, Bin}, State = #state{socket = Socket}) ->
	case gen_tcp:send(Socket, Bin) of
		ok -> skip;
		{error, Reason} ->
			io:format("rpc error :~w~n",[Reason])
	end,
	{noreply, State};
handle_cast(close, State = #state{socket = Socket, recv_pid = RecvPid}) ->
	gen_tcp:close(Socket),
	erlang:exit(RecvPid, kill),
	{noreply, State#state{socket = undefined, recv_pid = undefined}};
handle_cast(_Request, State) ->
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

handle_info(recv_down, State = #state{socket = undefined}) ->
	{noreply, State};
handle_info(recv_down, State = #state{socket = Socket}) ->
	OwnerPid = self(),
	RecvPid = spawn(fun() -> loop_recv(Socket, OwnerPid) end),
	{noreply, State#state{recv_pid = RecvPid}};
handle_info(ping, State = #state{socket = undefined}) ->
	erlang:send_after(10000, self(), ping),
	{noreply, State};
handle_info(_Info, State) ->
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
%% 循环收包并解包
loop_recv(Socket, OwnerPid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Context} ->
			io:format("xxxxx ~ts~n", [Context]),
			loop_recv(Socket, OwnerPid);
		{error, Reason} ->
			io:format("~p ~p~n", [?MODULE, Reason]),
			erlang:send_after(5000, OwnerPid, recv_down)
	end.
