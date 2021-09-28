%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 数据库
%%% @end
%%% Created : 09. 十一月 2020 20:06
%%%-------------------------------------------------------------------
-module(sys_db).
-author("liaoxifeng").
-include("common.hrl").
-include("role.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_kv/1,
    set_kv/2
]).

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

get_kv(Key) ->
    case ets:lookup(kv, Key) of
        [Value] -> Value;
        _ -> ?undefined
    end.

set_kv(Key, Value) ->
    ets:insert(kv, #kv{key = Key, value = Value, dirty = true}).

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
    ets:new(kv, [set, public, {keypos, #kv.key}, {read_concurrency, true}, named_table]),
    ets:new(role_brief, [set, public, {keypos, #role_brief.id}, {read_concurrency, true}, named_table]),
    dets:open_file(role, [{file, "../dets/role.dets"}, {keypos, #role.id}, {type, set}]),
    dets:open_file(role_brief, [{file, "../dets/role_brief.dets"}, {keypos, #role_brief.id}, {type, set}]),
    dets:open_file(kv, [{file, "../dets/kv.dets"}, {keypos, #kv.key}, {type, set}]),
    ?info("sys_db start success!!!"),
    erlang:send_after(?sync_dirty_interval, self(), sync_dirty),
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
handle_info(sync_dirty, State) ->
    sync_dirty(sync, kv),
    erlang:send_after(?sync_dirty_interval, self(), sync_dirty),
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
    ?info("[~w] 正在关闭...", [?MODULE]),
    sync_dirty(sync, kv),
    util:close_dets(role),
    util:close_dets(kv),
    ?info("[~w] 关闭完成", [?MODULE]),
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
%% 从ets写回dets
sync_dirty(Flag, kv) ->
    Fun = fun (#kv{dirty = false}, Acc) -> Acc;
        (E = #kv{key = Key, dirty = true}, sync) ->
            dets:insert(kv, E#kv{dirty = false}),
            ets:update_element(kv, Key, {#kv.dirty, false}),
            sync;
        (E = #kv{dirty = true}, close) ->
            dets:insert(kv, E#kv{dirty = false}),
            close
          end,
    ets:foldl(Fun, Flag, kv);
sync_dirty(_Flag, _Table) ->
    ok.