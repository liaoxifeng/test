%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 十一月 2021 15:59
%%%-------------------------------------------------------------------
-module(ekcp_session).
-author("liaoxifeng").
-include("ekcp.hrl").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([start_link/4, send/1]).

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
-spec send(Packet) -> Ret when
    Packet :: binary(),
    Ret :: integer().
send(Packet) ->
    try
        #ekcp_conf{
            snd_queue_threshold = MaxWaitNum,
            send_nodelay = P
        } = get(ekcp_config),
        KCP = get(ekcp),
        WaitNum = ekcp:waitsnd(KCP),
        if
            WaitNum > MaxWaitNum ->
                throw(max_wait_send);
            true ->
                Ret = ekcp:send(KCP, Packet),
                P andalso ekcp:flush(KCP),
                Ret
        end
    catch
        _Error:_Reason ->
            ?EKCP_SEND_EAGAIN
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(ConvId, UdpPid, Handler, EKCPConf) ->
    gen_server:start_link(?MODULE, [ConvId, UdpPid, Handler, EKCPConf], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% Send data from application layer to kcp layer.

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

init([ConvId, UdpPid, Handler, EKCPConf]) ->
    process_flag(trap_exit, true),
    try
        KCP = ekcp:create(ConvId, UdpPid),
%%        ekcp:nodelay(KCP, Nodelay, Interval, Resend, NC),
%%        ekcp:wndsize(KCP, SndWnd, RcvWnd),
%%        ekcp:setmtu(KCP, Mtu),
        put(ekcp_conv_id, ConvId),
        put(ekcp, KCP),
        put(ekcp_handle_module, Handler),
        put(ekcp_udp_pid, UdpPid),
        put(ekcp_start_ms, erlang:system_time(milli_seconds)),
        put(ekcp_config, EKCPConf),
        process_flag(trap_exit, true),
        {ok, State} = Handler:init([]),
        self() ! ekcp_update,
        {ok, State}
    catch
        _Error:Reason ->
            {stop, Reason}
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
handle_info(ekcp_recv, State) ->
    try
        KCP = get(ekcp),
        case ekcp:recv(KCP) of
            <<>> ->
                {noreply, State};
            Data ->
                self() ! ekcp_recv,
                handle_info({ekcp_recv, Data}, State)
        end
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
handle_info({ekcp_input, Packet}, State) ->
    try
        KCP = get(ekcp),
        ekcp:input(KCP, Packet),
        self() ! ekcp_recv,
        {noreply, State}
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
handle_info(ekcp_update, State) ->
    try
        KCP = get(ekcp),
        StartMS = get(ekcp_start_ms),
        NowMS = erlang:system_time(milli_seconds) - StartMS,
        ekcp:update(KCP, NowMS),
        NextMS = ekcp:check(KCP, NowMS),
        erlang:send_after(max(0, NextMS - NowMS), self(), ekcp_update),
        {noreply, State}
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
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
%%    catch ekcp:release(KCP),
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
