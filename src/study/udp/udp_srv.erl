%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 服务端 包最小(46~64字节) 最大64KB, 最好将UDP的数据长度控制在548字节以内, 超过1472字节就分片处理
%%%
%%% list | binary | {mode, list | binary} 以列表还是字符串的形式接收Packet。
%%% {ip, Address} 当host有多个网络接口的时候，选择其中一个。
%%% {ifaddr, Address} 和 {ip, Address} 一样的。
%%% {fd, integer() >= 0} 如果有socket不是使用 gen_udp 来打开的，那么就可能需要设置一个文件描述符。
%%% inet6 | inet | local 设置socket的类型。
%%% {udp_module, module()} 覆盖默认的udp模块。
%%% {multicast_if, Address} 为多播socket设置本地设备。
%%% {multicast_loop, true | false} 为真时，多播的packets会循环地返回到本地socket。
%%% {multicast_ttl, Integer} 多播的TTL，默认是1.
%%% {add_membership, {MultiAddress, InterfaceAddress}} 加入多播群。
%%% {drop_membership, {MultiAddress, InterfaceAddress}} 离开多播群。
%%% {active, true | false | once | N} 如果为真，socket收到的所有消息会发送到归属进程。如果为假，就需要显式调用recv；once 是收到消息后，就会发送到进程，但会变为false。数值是指接收多少条数据。
%%% {buffer, Size} 用户层级的缓冲区大小。
%%% {delay_send, Boolean} 通常erlang会立刻发出给socket的消息，开启这个选项后，会等待一会儿然后集合发出。
%%% {deliver, port | term} 发送给归属进程的消息的格式。
%%% {dontroute, Boolean} 对于发出的消息是否采用路由。
%%% {exit_on_close, Bloolean} socket 关闭时退出归属进程。
%%% {header, Size} 只有当binary起作用时才有用，单位是byte。
%%% @end
%%% Created : 23. 十月 2020 10:05
%%%-------------------------------------------------------------------
-module(udp_srv).
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
    process_flag(trap_exit, true),
    Port = 20005,
    TcpOptions = [
        binary,
        {active, true}
        ,{recbuf, 16*1024}
    ],
    {ok, LSock} = gen_udp:open(Port, TcpOptions),
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
handle_info({udp, _Socket, Host, Form, Msg}, #state{sock = Socket} = State) ->
    ?info("udp received Form ~w， len ~w", [Form, erlang:byte_size(Msg)]),
    gen_udp:send(Socket, Host, Form, <<"hello">>),
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

terminate(Reason, _State) ->
    ?info("~w~n", [Reason]),
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
    ?info("~w~n", [State]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================