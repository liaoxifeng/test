%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% test
%%% @end
%%% Created : 22. 十月 2020 10:48
%%%-------------------------------------------------------------------
-author("liaoxifeng").

-define(send_url, "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=ca122391-d1f8-491a-8903-4c19cf93c5ec").
-define(file_url, "https://qyapi.weixin.qq.com/cgi-bin/webhook/upload_media?key=ca122391-d1f8-491a-8903-4c19cf93c5ec&type=file").


-define(warn(Msg), lager:warning(Msg)).
-define(warn(F, Msg), lager:warning(io_lib:format(F, Msg)).
-define(info(Msg), lager:info(Msg)).
-define(info(F, Msg), lager:info(io_lib:format(F, Msg))).
-define(error(Msg), lager:error(Msg)).
-define(error(F, Msg), lager:error(io_lib:format(F, Msg))).