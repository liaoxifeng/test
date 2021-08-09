%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十月 2020 11:23
%%%-------------------------------------------------------------------
-module(zone).
-author("liaoxifeng").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------

init([]) ->

    AChild = [
        {acceptor_sup, {acceptor_sup, start_link, []}, permanent, 10000, supervisor, [acceptor_sup]}
        ,{listener, {listener, start_link, []}, transient, 100000, worker, [listener]}
        ,{webhook, {webhook, start_link, []}, transient, 100000, worker, [webhook]}
        ,{crontab, {crontab, start_link, []}, transient, 100000, worker, [crontab]}
        ,{sys_db, {sys_db, start_link, []}, transient, 100000, worker, [sys_db]}
    ],
    %% {RestartStrategy, MaxR, MaxT}, 如果在最近的MaxT秒之内有超过MaxR次数的重启，则supervisor停止它本身和它所有的子进程
    %% 当supervisor停止后，下一个更高级别的supervisor进行下一步动作，重启该停止的supervisor或者终止本身
    %% 重启机制的意图是防止一个进程由于某些原因重复性的死掉

    %% {Id, StartFunc, Restart, Shutdown, Type, Modules}
    %% Id是用来让supervisor内部识别子规范的名字
    %% StartFunc定义了用来启动子进程的的方法，符合module-function-arguments tuple{M, F, A}
    %% 它应该调用supervisor:start_link，gen_server:start_link，gen_fsm:start_link或gen_event:start_link，或相适应的方法
    %% Restart定义了子进程什么时候重启
    %% 1）permanent表示子进程始终重启
    %% 2）temporary表示子进程决不重启
    %% 3）transient表示只有在子进程异常终止时才重启，即除了normal以外的终止原因
    %% Shutdown定义了子进程怎样终止
    %% 1）brutal_kill表示子进程使用exit(Child, kill)来无条件的终止
    %% 2）一个整数timeout值表示supervisor告诉子进程通过调用exit(Child, shutdown)来终止，然后等待一个exit信号返回
    %% 如果没有在指定的时间内接收到exit信号，则子进程使用exit(Child, kill)来无条件的终止
    %% 3）如果子进程是另一个supervisor，它应该设置为infinity来给子树足够的时间来终止
    %% Type指定子进程是一个supervisor还是一个worker
    %% Modules应该是一个list，含有一个元素[Module]
    %% 如果子进程是一个supervisor，gen_server或gen_fsm则Module是callback模块的名字
    %% 如果子进程是一个gen_event，则Modules应该为dynamic
    %% 该信息用来在升级和降级时供release handler使用
    {ok, {{one_for_one, 50, 1}, AChild}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
