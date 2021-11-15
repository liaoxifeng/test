%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 服务端
%%% @end
%%% Created : 23. 十月 2020 10:05
%%%-------------------------------------------------------------------
-module(tcp_srv).
-author("liaoxifeng").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    port,
    sock
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
    Port = 20000,
    TcpOptions = [
        binary
        ,{packet, 0}
        ,{active, false}
        ,{reuseaddr, true}
        ,{nodelay, false}
        ,{delay_send, true}
        ,{exit_on_close, false}
        ,{send_timeout, 10000}
        ,{send_timeout_close, false}
    ],
    {ok, LSock} = gen_tcp:listen(Port, TcpOptions),
    self() ! loop,
    {ok, #state{port = Port, sock = LSock}}.

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
handle_info(loop, State = #state{sock = LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, spawn(fun () -> accept(Socket) end));
        {error, closed} -> ignore;
        {error, Reason} ->
            ?info("~p", [Reason])
    end,
    self() ! loop,
    {noreply, State};
handle_info(Info, State) ->
    ?info("~w~n", [Info]),
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
accept(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"ping">>} ->
            ?info("srv received ping"),
            gen_tcp:send(Socket, "pong"),
            accept(Socket);
        {ok, Str} ->
            ?info("~p", [Str]),
            accept(Socket);
        {error, closed} ->
            ?info("~p", [closed]),
            gen_tcp:close(Socket);
        {error, timeout} ->
            ?info("~p", [timeout]),
            ok;
        Else ->
            ?error("gen_tcp recv error ~p", [Else]),
            ok
    end.