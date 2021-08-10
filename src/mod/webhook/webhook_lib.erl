%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 群机器人工具
%%% @end
%%% Created : 29. 十月 2020 15:10
%%%-------------------------------------------------------------------
-module(webhook_lib).
-author("liaoxifeng").
-include("common.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
    webhook_text/3
    ,webhook_image/1
    ,webhook_file/2
    ,weather/0
    ,weather2/0
    ,weather3/0
    ,drink/0
]).


%% 发送文本
webhook_text(Context, [], []) ->
    #{
        msgtype => 'text',
        text => #{content => unicode:characters_to_binary(Context)}
    };
webhook_text(Context, MentionedL, []) ->
    #{
        msgtype => 'text',
        text => #{
            content => unicode:characters_to_binary(Context),
            mentioned_list => MentionedL
        }
    };
webhook_text(Context, MentionedL, MMobileL)
    when MentionedL /= [] andalso MMobileL /= [] ->
    #{
        msgtype => 'text',
        text => #{
            content => unicode:characters_to_binary(Context),
            mentioned_list => MentionedL,
            mentioned_mobile_list => MMobileL
        }
    };
webhook_text(_Context, _MentionedL, _MMobileL) ->
    #{}.

%% 上传文件
webhook_file(Url, Path) ->
    {ok, BinStream} = file:read_file(Path),
    Data = binary_to_list(BinStream),
    Boundary = "-------------------------acebdf13572468",
    ReqBody = format_form_data(Boundary, [{media, filename:basename(Path), Data}]),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    ReqHeader = [{"Content-Length", integer_to_list(length(ReqBody))}],
    case httpc:request(post, {Url, ReqHeader, ContentType, ReqBody}, [], []) of
        {ok, {_Status, _Headers, Body}} ->
            case jiffy:decode(Body, [return_maps]) of
                #{<<"errcode">> := 0, <<"media_id">> := MediaIdStr} ->
                    Map = #{
                        msgtype => 'file',
                        file => #{
                            media_id => MediaIdStr
                        }
                    },
                    SendBody = jiffy:encode(Map),
                    httpc:request(post, {?send_url, [], "application/json", SendBody}, [], []);
                _ ->
                    ?info("error")
            end;
        _ ->
            ?info("error")
    end.

format_form_data(Boundary, Files) ->
    FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
        [lists:concat(["--", Boundary]),
            lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\"; filename=\"",FileName,"\""]),
            lists:concat(["Content-Type: ", "application/octet-stream"]), "", FileContent]
                          end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([FileParts2, EndingParts]),
    string:join(Parts, "\r\n").


%% 发送图片
webhook_image(Path) ->
    {ok, BinStream} = file:read_file(Path),
    [H|_] = string:tokens(os:cmd("md5sum " ++ Path)," "),
    Base64 = base64:encode(BinStream),
    Map = #{
        msgtype => 'image',
        image => #{
            base64 => unicode:characters_to_binary(Base64),
            md5 => unicode:characters_to_binary(H)
        }
    },
    util:to_binary(jiffy:encode(Map)).


%% 官网接口
weather() ->
    case httpc:request(get, {"http://www.weather.com.cn/data/sk/101280101.html", []} , [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Headers, Body}} ->
            case jiffy:decode(Body, [return_maps]) of
                #{<<"weatherinfo">> := Map} ->
                    Body3 = maps:to_list(Map),
                    format(Body3, ""),
                    webhook:send_text(format(Body3, ""), ['@all']),
                    ok;
                _ ->
                    ok
            end;
        _ ->
            ignore
    end.

format([], R) ->
    L = [V || {_, V} <- lists:keysort(1, R)],
    lists:flatten(io_lib:format("~ts", [L]));
format([{<<"city">>, V} | Xml], R) ->
    V3 = ["城市：", V, "\n"],
    format(Xml, [{1, V3} | R]);
format([{<<"temp">>, V} | Xml], R) ->
    V3 = ["温度：", V, "\n"],
    format(Xml, [{2, V3} | R]);
format([{<<"WD">>, V} | Xml], R) ->
    V3 = ["风向：", V, "\n"],
    format(Xml, [{3, V3} | R]);
format([{<<"WSE">>, V} | Xml], R) ->
    V3 = ["风力：", V, "\n"],
    format(Xml, [{4, V3} | R]);
format([{<<"WS">>, V} | Xml], R) ->
    V3 = ["风速：", V, "\n"],
    format(Xml, [{5, V3} | R]);
format([{<<"SD">>, V} | Xml], R) ->
    V3 = ["相对湿度：", V, "\n"],
    format(Xml, [{6, V3} | R]);
format([{<<"time">>, V} | Xml], R) ->
    V3 = ["更新时间：", V, "\n"],
    format(Xml, [{7, V3} | R]);
format([_ | Xml], R) ->
    format(Xml, R).

%% 中华万年历API
%% Json
weather2() ->
    City = binary_to_list(unicode:characters_to_binary("广州")),
    CityStr = lists:append([io_lib:format("%~.16B", [E]) || E <- City]),
    case httpc:request(get, {"http://wthrcdn.etouch.cn/weather_mini?city=" ++ CityStr, []} , [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Headers, Body}} ->
            Json = binary_to_list(zlib:gunzip(Body)),
            case jiffy:decode(Json, [return_maps]) of
                #{<<"status">> := 1000, <<"data">> := Data} -> webhook:send_text(format2(Data));
                _ -> ok
            end;
        _ -> ignore
    end.

format2(#{<<"city">> := City, <<"ganmao">> := GanMao, <<"wendu">> := WenDu, <<"forecast">> := Forecast}) ->
    Part1 = ["城市：", City, "\n"],
    Part2 = ["日期：", util:format_date(), "\n"],
    Part3 = ["温馨提示：", GanMao, "\n"],
    Part4 = ["温度：", WenDu, "\n"],
    Part5 = lists:map(
        fun(#{<<"date">> := Date, <<"fengli">> := FL, <<"fengxiang">> := FX,
            <<"high">> := High, <<"low">> := Low, <<"type">> := Type}) ->
            [_,_,FL2|_] = string:tokens(unicode:characters_to_list(FL),"[ ]"),
            [_, Low2] = string:tokens(unicode:characters_to_list(Low)," "),
            [_, High2] = string:tokens(unicode:characters_to_list(High)," "),
            [Date, " ", Type, " ", "吹", FL2, FX, " ", "温度区间：", Low2, " - ", High2, "\n"]
        end, Forecast
    ),
    lists:flatten(io_lib:format("~ts", [lists:concat([Part1, Part2, Part3, Part4, Part5, "\n"])])).

%% 中华万年历API
%% XML
weather3() ->
    case httpc:request(get, {"http://wthrcdn.etouch.cn/WeatherApi?citykey=101280101", []} , [], []) of
        {ok, {{"HTTP/1.1",200,"OK"}, _Headers, Body}} ->
            Xml = binary_to_list(zlib:gunzip(Body)),
            decode(Xml),
            filter(decode(Xml)),
            ok;
        _ ->
            ignore
    end.

filter(L) ->
    [?info("~p, ~ts", [K, V]) || {K,V} <- L].

decode(Xml) ->
    ContentList = decode_shift(util:to_list(Xml), []),
    TextL =
        lists:foldl(
            fun(#xmlText{pos = Pos, parents = Parents, value = Value}, TextL1) ->
                {Key, _} = lists:nth(Pos, Parents),
                [{Key, Value} | TextL1]
            end, [], ContentList),
    TextL.

%% 转换内容的解析内容
decode_shift("", L) ->
    L;
decode_shift(Xml, L) ->
    {Doc, Doc1} = xmerl_scan:string(Xml),
    Content = decode_handler(Doc),
    decode_shift(Doc1, Content ++ L).

%%处理内容的解析内容
decode_handler(Element) ->
    lists:flatten(decode_handler(Element, [])).
decode_handler(Element, L) ->
    case Element of
        #xmlText{pos = 1} ->
            [Element | L];
        #xmlElement{content = Content1} ->
            decode_handler(Content1, []) ++ L;
        ElementList when is_list(ElementList) ->
            [decode_handler(Element1, []) || Element1 <- ElementList];
        _ ->
            L
    end.


%% 喝水
drink() ->
    Body = webhook_lib:webhook_image("../priv/drink.png"),
    httpc:request(post, {?send_url, [], "application/json", Body}, [], []),
    ok.