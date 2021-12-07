%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 定时器
%%% @end
%%% Created : 09. 十一月 2020 20:06
%%%-------------------------------------------------------------------
-module(crontab).
-author("liaoxifeng").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add/1, del/1
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
%% 添加定时器
add(Time) when is_tuple(Time) -> add([Time]);
add(L) when is_list(L) ->
    L2 = [{Y,M,D,H,Min,S,{Mod,Fun,Args}} || {Y,M,D,H,Min,S,{Mod,Fun,Args}} <- L],
    ?MODULE ! {add_time, L2};
add(_) -> ignore.

%% 删除定时器
del(Time) -> ?MODULE ! {del_time, Time}.
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
    Ref = erlang:send_after(1000, self(), check),
    ?info("crontab start success!!!"),
    {ok, #{ref => Ref, rule => rule()}}.

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
handle_info(check, #{ref := Ref, rule := Rules} = State) ->
    erlang:cancel_timer(Ref),
    {{Year, Mon, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    hdl(Rules, {Year, Mon, Day, Hour, Min, Sec}),
    Ref2 = erlang:send_after(1000, self(), check),
    {noreply, State#{ref => Ref2}};
handle_info({add_time, L}, #{rule := Rules} = State) ->
    Rules2 = lists:umerge(L, Rules),
    {noreply, State#{rule := Rules2}};
handle_info({del_time, Time}, #{rule := Rules} = State) ->
    Rules2 = lists:delete(Time, Rules),
    {noreply, State#{rule := Rules2}};
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
%% 年 月 日 时 分 秒
rule() -> [
    {"*", "*", "*", 6, 0, 0, {webhook, weather, []}},
    {"*", "*", "*", [9,10,11,13,14,15,16,17,19], 0, 0, {webhook_lib, drink, []}},
    {"*", "*", "*", [12,18], 0, 0, {webhook_lib, eat, []}},
    {"*", "*", "*", [20], 0, 0, {webhook_lib, leave, []}}
].

%% 处理定时器
hdl([], _Check) -> ok;
hdl([Rule | T], Check) ->
    case check([year, month, day, hour, min, sec], Rule, Check) of
        {true, M, F, A} -> erlang:apply(M, F, A);
        _ -> ok
    end,
    hdl(T, Check).

%% 检查定时器
check([], {_Year, _Mon, _Day, _Hour, _Min, _Sec, {M, F, A}}, _Check) -> {true, M, F, A};
check([year | T], {Year, _Mon, _Day, _Hour, _Min, _Sec, _Mfa} = Rule, {YearC, _MonC, _DayC, _HourC, _MinC, _SecC} = Check) ->
    case check_rule(Year, YearC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check([month | T], {_Year, Mon, _Day, _Hour, _Min, _Sec, _Mfa} = Rule, {_YearC, MonC, _DayC, _HourC, _MinC, _SecC} = Check) ->
    case check_rule(Mon, MonC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check([day | T], {_Year, _Mon, Day, _Hour, _Min, _Sec, _Mfa} = Rule, {_YearC, _MonC, DayC, _HourC, _MinC, _SecC} = Check) ->
    case check_rule(Day, DayC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check([hour | T], {_Year, _Mon, _Day, Hour, _Min, _Sec, _Mfa} = Rule, {_YearC, _MonC, _DayC, HourC, _MinC, _SecC} = Check) ->
    case check_rule(Hour, HourC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check([min | T], {_Year, _Mon, _Day, _Hour, Min, _Sec, _Mfa} = Rule, {_YearC, _MonC, _DayC, _HourC, MinC, _SecC} = Check) ->
    case check_rule(Min, MinC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check([sec | T], {_Year, _Mon, _Day, _Hour, _Min, Sec, _Mfa} = Rule, {_YearC, _MonC, _DayC, _HourC, _MinC, SecC} = Check) ->
    case check_rule(Sec, SecC) of
        true -> check(T, Rule, Check);
        _ -> false
    end;
check(_L, _Rule, _Check) -> false.

check_rule("*", _Check) -> true;
check_rule(Rule, Check) when is_list(Rule) -> lists:member(Check, Rule);
check_rule(Rule, Check) when is_integer(Rule) -> Rule =:= Check;
check_rule(_Rule, _Check) -> false.