%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% webhook 机器人
%%% @end
%%% Created : 28. 十月 2020 11:08
%%%-------------------------------------------------------------------
-module(webhook).
-author("liaoxifeng").
-include("common.hrl").

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    send_text/1,
    send_text/2,
    send_text/3,
    send_image/0,
    post_file/1,
    weather/0
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

%% @doc 发送text文本
%% webhook:send_text("下班啦",['@all'], ['13114379105']).
send_text(Context) -> send_text(Context, [], []).
send_text(Context, MentionedL) -> send_text(Context, MentionedL, []).
send_text(Context, MentionedL, MMobileL) ->
    ?MODULE ! {text, Context, MentionedL, MMobileL},
    ok.

%% @doc 发送图片
send_image() ->
    {ok, FileNames} = file:list_dir("../priv/face"),
    FileName = util:rand_list(FileNames),
    send_image("../priv/face/" ++ FileName).
send_image(Path) ->
    ?MODULE ! {image, Path},
    ok.

%% @doc 上传文件
post_file(Path) ->
    ?MODULE ! {post_file, Path},
    ok.

%% @doc 定时推送天气预报
weather() -> webhook_lib:weather2().

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
    code:load_file(webhook_lib),
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

handle_info({text, Context, MentionedL, MMobileL}, State) ->
    Map = webhook_lib:webhook_text(Context, MentionedL, MMobileL),
    Body = jiffy:encode(Map),
    httpc:request(post, {?send_url, [], "application/json", Body} , [], []),
    {noreply, State};
handle_info({image, Path}, State) ->
    Body = webhook_lib:webhook_image(Path),
    httpc:request(post, {?send_url, [], "application/json", Body}, [], []),
    {noreply, State};
handle_info({post_file, Path}, State) ->
    webhook_lib:webhook_file(?file_url, Path),
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