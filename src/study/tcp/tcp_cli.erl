%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 客户端测试机
%%% @end
%%% Created : 23. 十月 2020 9:05
%%%-------------------------------------------------------------------
-module(tcp_cli).
-author("liaoxifeng").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, connect/0, rpc/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {
	socket,
	pid
}).

%%%===================================================================
%%% API
%%%===================================================================

%% 请求连接
connect() ->
	connect("192.168.31.13", 20000).
connect(Host, Port) ->
	gen_server:cast(?MODULE, {connect, Host, Port}).

rpc(String) ->
	Bin = unicode:characters_to_binary(String),
	gen_server:cast(?MODULE, {rpc, Bin}).

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
	connect(),
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
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({connect, Host, Port}, State) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}]),
	Pid = spawn(fun() -> loop_recv(Socket) end),
	gen_tcp:controlling_process(Socket, Pid),
	{noreply, State#state{socket = Socket, pid = Pid}};
handle_cast({rpc, Bin}, State = #state{socket = Socket}) ->
	case gen_tcp:send(Socket, Bin) of
		ok -> skip;
		{error, Reason} ->
			io:format("rpc error :~w",[Reason])
	end,
	{noreply, State};
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


handle_info(ping, State) ->
	erlang:send_after(10000, self(), ping),
	rpc("ping"),
	{noreply, State};

handle_info(_Info, State) ->
	?info("xxxxx ~ts", [_Info]),
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
loop_recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Context} ->
			?info("cli received ~ts", [Context]),
			loop_recv(Socket);
		{error, Reason} ->
			?error("~p ~w", [?MODULE, Reason]),
			ok
	end.
