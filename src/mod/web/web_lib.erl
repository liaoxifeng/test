%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十一月 2020 14:16
%%%-------------------------------------------------------------------
-module(web_lib).
-author("liaoxifeng").
-include("test.hrl").


%% API
-export([web_hdl/2]).

web_hdl({ok, Request}, Socket) ->
    L = binary_to_list(Request),
    [H|T] = string:tokens(L, "\r\n"),
    [Method, Url|_] = string:tokens(H, " "),
    Map = parse(T),
    Req = web_hdl2(Map#{"Method" => Method, "Url" => Url}),
    gen_tcp:send(Socket, response(Req)),
    ok ;
web_hdl(_Else, Socket) ->
    gen_tcp:send(Socket, response("hello world")),
    ok.

web_hdl2(#{"Method" := "GET", "Url" := "/"}) ->
    {ok, BinStream} = file:read_file("../priv/html/index.html"),
    BinStream;
web_hdl2(#{"Method" := "POST", "Url" := "/login", "Body" := Body} = M) ->
    ?info("~p", [M]),
    Body;
web_hdl2(Map) ->
    ?error("error ~p", [Map]),
    "error".



response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s", [size(B), B])).

parse(Str) -> parse(Str, []).
parse([], Req) -> maps:from_list(Req);
parse([Str|T], Req) ->
    case string:tokens(Str, ": ,") of
        ["Host", Ip, Port] ->
            parse(T, [{"Ip", Ip}, {"Port", Port} | Req]);
        ["Referer", Http, Url] ->
            parse(T, [{"Referer", lists:concat([Http, ":", Url])} | Req]);
        ["Origin", Http, Url] ->
            parse(T, [{"Origin", lists:concat([Http, ":", Url])} | Req]);
        ["Accept" | Tail] ->
            parse(T, [{"Accept", Tail} | Req]);
        ["Accept-Language" | Tail] ->
            parse(T, [{"Accept-Language", Tail} | Req]);
        ["Accept-Encoding", Zip, Style] ->
            parse(T, [{"Accept-Encoding", {Zip, Style}} | Req]);
        ["User-Agent"|_] ->
            parse(T, Req);
        [Key, Value] ->
            parse(T, [{Key, Value} | Req]);
        _ ->
            parse(T, [{"Body", Str} | Req])
    end.