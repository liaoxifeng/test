%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十月 2020 10:05
%%%-------------------------------------------------------------------
-module(acceptor).
-author("liaoxifeng").
-include("test.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(LSock) ->
    gen_server:start_link(acceptor, [LSock], []).

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

init([LSock]) ->
    self() ! loop,
    {ok, {LSock}}.

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
handle_info(loop, State = {LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            gen_tcp:controlling_process(Socket, spawn(fun () -> accept(Socket) end));
        {error, closed} -> ignore;
        {error, Reason} ->
            ?info("~p", [Reason])
    end,
    self() ! loop,
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
%%  gen_tcp:send(Socket, ).
accept(Socket) ->
    case gen_tcp:recv(Socket, 23) of
        {ok, <<"game_client------------">>} ->
            ?info("~p", [game_client]),
            create_conn(game, 4, Socket);
        {error, closed} ->
            ?info("~p", [closed]),
            gen_tcp:close(Socket);
        {error, timeout} ->
            ?info("~p", [timeout]),
            ok;
        Else ->
            ?info("Else ~p", [Else]),
            web_lib:web_hdl(Else, Socket)
    end.

create_conn(ClientType, _Packet, Socket) ->
    try
        {ok, {Ip, Port}} = inet:peername(Socket),
        ?info("~p ~w", [Ip, Port]),
        {ok, Pid} = conn:create(ClientType, Socket, Ip, Port),
        gen_tcp:controlling_process(Socket, Pid)
    catch
        _:_ ->
            gen_tcp:close(Socket)
    end.
