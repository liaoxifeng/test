%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% demo_1 terminate与trap_exit
%%% @end
%%% Created : 09. 八月 2021 17:06
%%%-------------------------------------------------------------------
-module(process2).
-author("liaoxifeng").

-behaviour(gen_server).

%% API
-export([start_link/0, exit/1, crash/1]).

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

%% link的进程
%% demo1:exit(normal).   process_flag(trap_exit, true)   不执行terminate handle_info/2 收到 {'EXIT',<0.86.0>,normal} 进程还在
%% demo1:exit(kill).     process_flag(trap_exit, true)   不执行terminate 不收到消息，进程挂了
%% demo1:exit(whatever). process_flag(trap_exit, true)   不执行terminate handle_info/2 收到 {'EXIT',<0.86.0>,whatever} 进程还在
%% demo1:exit(normal).   process_flag(trap_exit, false)  不执行terminate 不收到消息，进程还在
%% demo1:exit(kill).     process_flag(trap_exit, false)  不执行terminate 不收到消息，进程挂了
%% demo1:exit(whatever). process_flag(trap_exit, false)  不执行terminate 不收到消息，进程挂了
exit(Reason) ->
    exit(whereis(demo1), Reason).

%% crash(1).   process_flag(trap_exit, true)  执行terminate 进程挂了
%% crash(1).   process_flag(trap_exit, false)  执行terminate 进程挂了
crash(N) ->
    ?MODULE ! {crash, N}.
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
    process_flag(trap_exit, true),
%%    process_flag(trap_exit, false),
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
handle_info({crash, N}, State) ->
    io:format("[line:~p] crash ~w~n", [?LINE, N/0]),
    {noreply, State};
handle_info(stop, State) ->
    io:format("[line:~p] [info] ~w~n", [?LINE, stop]),
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("[line:~p] [info] ~w~n", [?LINE, Info]),
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
    io:format("[line:~p] [terminate] ~w~n", [?LINE, Reason]),
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
