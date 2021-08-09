%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% role socket收发器
%%% @end
%%% Created : 30. 十一月 2020 9:08
%%%-------------------------------------------------------------------
-module(role_conn).
-author("liaoxifeng").
-include("common.hrl").
-include("role.hrl").

-behaviour(gen_server).

%% API
-export([create/4, rpc/2, send/2]).

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
    pid = ?undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

rpc(Pid, String) ->
    Bin = list_to_binary(String),
    Pid ! {rpc, Bin}.

%% 信息返回客户端
send(#role{sid = Sid}, Bin) ->
    Sid ! {send, Bin};
send(Sid, Bin) ->
    Sid ! {send, Bin}.

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
    prim_inet:async_recv(Socket, 0 , 60000),
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
    ?info("~w~n", [Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(Request, State) ->
    ?info("~w~n", [Request]),
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
handle_info({rpc, Bin}, State = #conn{socket = Socket}) ->
    case gen_tcp:send(Socket, Bin) of
        ok -> skip;
        {error, Reason} ->
            ?error("rpc error :~w~n",[Reason])
    end,
    {noreply, State};

handle_info({inet_async, Socket, _Ref, {ok, Msg}}, State) ->
    prim_inet:async_recv(Socket, 0 , 60000),
    case conn_hdl(Msg, State) of
        State2 when is_record(State2, conn) ->
            {noreply, State2};
        _ ->
            {noreply, State}
    end;

handle_info({inet_async, _Socket, _Ref, {error, timeout}}, State) ->
    {stop, normal, State};
handle_info({inet_async, _Socket, _Ref, {error, closed}}, State) ->
    {stop, normal, State};
handle_info({send, Bin}, #conn{socket = Socket} = State) ->
    gen_tcp:send(Socket, Bin),
    {noreply, State};
handle_info(Info, State) ->
    ?info("~w", [Info]),
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

terminate(Reason, _State) ->
    ?info("Reason ~w", [Reason]),
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
%% 客户端协议处理
conn_hdl(Msg, #conn{pid = RPid}) ->
    ?info("~w", [Msg]),
    case is_process_alive(RPid) of
        true ->
            RPid ! {apply_async, {conn_hdl, do, [Msg]}};
        _ ->
            ok
    end.