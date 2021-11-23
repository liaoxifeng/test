%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十一月 2021 15:59
%%%-------------------------------------------------------------------
-module(ekcp_service).
-author("liaoxifeng").
-include("ekcp.hrl").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([start_link/3]).

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
    socket,
    ekcp_conf
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

start_link(Port, UDPOpts, EKCPConf) ->
    gen_server:start_link(ekcp_service, [Port, UDPOpts, EKCPConf], []).

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

init([Port, Options, EKCPConf]) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, Options) of
        {ok, Socket} ->
            {ok, #state{
                port = Port,
                socket = Socket,
                ekcp_conf = EKCPConf
            }};
        {error, Reason} ->
            ?error("ekcp_service start failure [~w]", [Reason]),
            {stop, normal}
    end.

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

handle_info({udp, Socket, Ip, Port, Packet}, State) ->
%%    ekcp_session_sup:start_child(ConvId, self(), Handler, EKCPConf),
%%    ekcp:getconv(Packet),
    {noreply, State};
handle_info({ekcp_output, Packet}, #state{socket = Socket} = State) ->
%%    gen_udp:send(Socket, Ip, Port, Packet),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, #state{} = State) ->
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

terminate(_Reason, State) ->
    catch gen_udp:close(State#state.socket),
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
