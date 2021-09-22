%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 工具
%%% @end
%%% Created : 29. 十月 2020 15:10
%%%-------------------------------------------------------------------
-module(util).
-author("liaoxifeng").
-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
    to_list/1
    ,rand_list/1
    ,format_date/0
    ,to_binary/1
    ,unixtime/0
    ,md5/1
    ,string_to_term/1
    ,close_dets/1
]).

%%%% 打包协议
%%encode_msg(Msg) -> encode_msg(Msg, []).
%%encode_msg(Msg, Opts) ->
%%    Body = all_pb:encode_msg(Msg, Opts),
%%    Name = atom_to_binary(element(1, Msg), latin1),
%%    NameLen = byte_size(Name),
%%    <<NameLen:8, Name/binary, Body/binary>>.
%%
%%%% 解包协议
%%decode_msg(<<NameLen:8, NameBin:NameLen/binary, Body/binary>>) ->
%%    MsgName = binary_to_atom(NameBin, latin1),
%%    all_pb:decode_msg(Body, MsgName).


%% @doc term反序列化，string转换为term
-spec string_to_term(String) -> {error, Reason} | {ok, term()} when
    String :: undefined | string() | bitstring(),
    Reason :: term().
string_to_term(undefined) -> {ok, undefined};
string_to_term("undefined") -> {ok, undefined};
string_to_term(String) when is_bitstring(String) ->
    string_to_term(binary_to_list(String));
string_to_term(String) ->
    S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [{return, list}, global]),
    case erl_scan:string(S ++ ".") of
        {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
        {error, Err, _} -> {error, Err}
    end.

%% @doc 生成16位格式的md5值
-spec md5(iodata()) -> binary().
md5(Data) ->
    list_to_binary([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(Data))]).

%% @doc 取得当前的unix时间戳
-spec unixtime() -> pos_integer().
unixtime() ->

    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% @doc 从一个list中随机取出一项
-spec rand_list(List::list()) -> undefined | term().
rand_list([]) -> undefined;
rand_list([I]) -> I;
rand_list(List) ->
    Idx = rand(1, length(List)),
    get_term_from_list(List, Idx).
get_term_from_list([H | _T], 1) -> H;
get_term_from_list([_H | T], Idx) ->
    get_term_from_list(T, Idx - 1).

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand(Min::integer(), Max::integer()) -> integer().
rand(Min, Max) when Max < Min ->
    rand(Max, Min);
rand(Min, Min) -> Min;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    %% @todo 这个机制有必要改进下
    case get(rand_seed) of
        undefined ->
            case crypto:strong_rand_bytes(12) of
                <<A:32,B:32,C:32>> ->
                    rand:seed(exsplus, {A,B,C}),
                    ok;
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.

%% @doc 格式化日期
format_date() ->
    {Y, M, D} = erlang:date(),
    lists:concat([Y, "年", M, "月", D, "日"]).

%% @doc to list
to_list(V) when is_list(V) -> V;
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V).

%% @doc to binary
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, unicode);
to_binary(V) when is_binary(V) -> V.

%% @doc 关闭dets，有错则打印
close_dets(Name) ->
    case catch dets:close(Name) of
        ok -> ok;
        _Err -> ?error("关闭dets[~w]失败:~w", [Name, _Err])
    end.