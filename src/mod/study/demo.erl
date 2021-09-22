%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% terminate与trap_exit
%%% @end
%%% Created : 10. 八月 2021 10:35
%%%-------------------------------------------------------------------
-module(demo).
-author("liaoxifeng").

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/1, crash/1, print/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
stop(Reason) ->
    ?MODULE ! {stop, Reason}.

crash(N) ->
    ?MODULE ! {crash, N}.

print() ->
    ?MODULE ! print.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start(Trap) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Trap], []).

start_link(Trap) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Trap], []).

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

init([Trap]) ->
    erlang:process_flag(trap_exit, Trap),
    {ok, #{}}.

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

handle_info(print, State) ->
    io:format("[line:~p] print ~p~n", [?LINE, self()]),
    {noreply, State};
handle_info({crash, N}, State) ->
    io:format("[line:~p] crash ~w~n", [?LINE, N/0]),
    {noreply, State};
handle_info({stop, Reason}, State) ->
    io:format("[line:~p] stop by ~p ~n", [?LINE, Reason]),
    {stop, Reason, State};
handle_info(Info, State) ->
    io:format("[line:~p] info ~p~n", [?LINE, Info]),
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
    io:format("[line:~p] Terminate reason: ~p~n", [?LINE, Reason]),
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

%% demo:start_link(false).
%% demo:crash(2).
%% demo:start_link(false).
%% demo:stop(normal).  demo:stop(whatever).
%%
%% gen_server 内部自己退出或发生crash退出，都会执行terminate/2
%% 如果stop的原因不是normal，error_log会记录本次退出信息。

%% gen_server 外部强制退出
%% demo:start_link(false).
%% demo:start_link(true).
%%
%% 使用erlang:exit(Pid, kill). 强制发送退出信号，terminate/2不会执行。
%%
%% demo:start_link(false).
%% erlang:exit(Pid, normal).   不执行terminate/2，  进程还在
%% erlang:exit(Pid, whatever). 不执行terminate/2，  进程挂了
%%
%% demo:start_link(true).
%% erlang:exit(Pid, normal).   执行terminate/2， 进程挂了
%% erlang:exit(Pid, whatever). 执行terminate/2， 进程挂了
%%
%%
%% demo:start_link(false). true
%% demo ! {'EXIT',self(), whatever}.   执行terminate/2， 进程挂了，错误打印
%% demo ! {'EXIT',self(), normal}.     执行terminate/2， 进程挂了，无错误打印
%%
%% demo:start(false). true
%% demo ! {'EXIT',self(), whatever}. handle_info/2 收到 {'EXIT',<0.333.0>,whatever}
%% demo ! {'EXIT',self(), normal}.   handle_info/2 收到 {'EXIT',<0.333.0>,normal}
%%
%%
%% trap_exit: fasle    使用exit(Pid, Reason) 强制发送退出信号，terminate/2并不会执行。
%% trap_exit: true     使用exit(Pid, Reason) 强制发送退出信号，terminate/2会执行。
%% {'EXIT',Pid,Reason} 消息发送给gen_server:start/3启动的进程，消息被当成普通的消息被handle_info/2处理。
%% {'EXIT',Pid,Reason} 消息发送给gen_server:start_link/3启动的进程，消息被当成退出信号被terminate/2处理。