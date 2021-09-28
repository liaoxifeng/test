%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 排行榜服务
%%% @end
%%% Created : 22. 九月 2021 16:07
%%%-------------------------------------------------------------------
-module(rank).
-author("liaoxifeng").
-include("rank.hrl").
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

-export([sort/1, sort/2]).
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [?start_opt_hibernate]).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(calc_next_time(), self(), sort),
    ets:new(rank, [set, named_table, public,{read_concurrency, true}, {keypos, #rank.rank_type}]),
    ets:new(rank_data, [set, named_table, public,{read_concurrency, true}, {keypos, #rank_data.key}]),
    dets:open_file(rank_data, [{file, "../dets/rank_data.dets"}, {keypos, #rank_data.key}]),
    ver_update(),
    erlang:send_after(?sec_ms, self(), sync_dirty),
    self() ! sort_now,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(init, State) ->
    sort(?rank_list_all),
    {noreply, State};
handle_info(sync_dirty, State) ->
    sync_dirty(sync, rank_data),
    erlang:send_after(?sec_ms, self(), sync_dirty),
    {noreply, State};
handle_info(sort, State) ->
    erlang:send_after(calc_next_time(), self(), sort),
    sort(?rank_list_all),
    {noreply, State};
handle_info({sort, RankType}, State) ->
    sort([RankType]),
    {noreply, State};
handle_info(sort_now, State) ->
    sort(?rank_list_all),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ?info("[~w] 正在关闭", [?MODULE]),
    dets:from_ets(rank_data, rank_data),
    util:close_dets(rank_data),
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    supervisor:terminate_child(sup_zone, rank),
    supervisor:restart_child(sup_zone, rank),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 计算下个刷新时刻
calc_next_time() ->
    case ((util:unixtime() - util:unixtime(zero)) rem ?rank_sort_interval) of
        0 -> ?rank_sort_interval;
        Sec -> ?rank_sort_interval - Sec
    end.

%% 排序
sort([]) -> ok;
sort([RankType | RankList]) ->
    case ets:match_object(rank_data, #rank_data{rank_type = RankType, _ = '_'}) of
        RankData when is_list(RankData) ->
            SortData = sort_data(RankType, RankData),
            NoOrder = [E#rank_data{index = 0} || E <- SortData],
            #rank_cfg{len = Len} = rank_cfg:get(RankType),
            {Top, Bottom} = ?IF(length(NoOrder) > Len, lists:split(Len, NoOrder), {NoOrder, []}),
            OrderRank = util:add_index(Top, #rank_data.index),
            ets:insert(rank, #rank{rank_type = RankType, list = OrderRank}),
            ets:insert(rank_data, OrderRank),
            ets:insert(rank_data, Bottom),
            sort(RankList);
        _ ->
            sort(RankList)
    end.

sort_data(RankType, RankData) ->
    case rank_cfg:get(RankType) of
        #rank_cfg{sort_rule = SortRule} ->
            sort(SortRule, RankData);
        _ -> []
    end.

sort(Rule, RankData) ->
    Fun =
        fun(A, B) ->
            sort2(A, B, Rule)
        end,
    lists:sort(Fun, RankData).

sort2(_, _, []) -> true;
sort2(A, B, [{SortPos, asc} | T]) ->
    ValA = element(SortPos, A),
    ValB = element(SortPos, B),
    if
        ValA < ValB -> true;
        ValA =:= ValB -> sort2(A, B, T);
        true -> false
    end;
sort2(A, B, [{SortPos, desc} | T]) ->
    ValA = element(SortPos, A),
    ValB = element(SortPos, B),
    if
        ValA > ValB -> true;
        ValA =:= ValB -> sort2(A, B, T);
        true -> false
    end.

%% 从ets写回dets
sync_dirty(Flag, rank_data) ->
    Fun =
        fun(#rank_data{dirty = false}, Accu) ->
            Accu;
            (E = #rank_data{key = Key, dirty = true}, sync) ->
                dets:insert(rank_data, E#rank_data{dirty = false}),
                ets:update_element(rank_data, Key, {#rank_data.dirty, false}),
                sync;
            (E = #rank_data{dirty = true}, close) ->
                dets:insert(rank_data, E#rank_data{dirty = false}),
                close
        end,
    ets:foldl(Fun, Flag, rank_data).

%% 版本处理
ver_update() -> ok.