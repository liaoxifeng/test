%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十月 2020 10:48
%%%-------------------------------------------------------------------
-author("liaoxifeng").

-define(send_url, "https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=ca122391-d1f8-491a-8903-4c19cf93c5ec").
-define(file_url, "https://qyapi.weixin.qq.com/cgi-bin/webhook/upload_media?key=ca122391-d1f8-491a-8903-4c19cf93c5ec&type=file").

-define(undefined, undefined).
-define(true, 1).
-define(false, 0).

%% 获取秒数值或毫秒数值
-define(week_sec, 604800).
-define(day_sec, 86400).
-define(hour_sec, 3600).
-define(half_hour_sec, 1800).
-define(minu_sec, 60).
-define(week_sec(N), (?week_sec * (N))).
-define(day_sec(N), (?day_sec * (N))).
-define(hour_sec(N), (?hour_sec * (N))).
-define(minu_sec(N), (?minu_sec * (N))).

-define(week_ms, 604800000).
-define(day_ms, 86400000).
-define(hour_ms, 3600000).
-define(half_hour_ms, 1800000).
-define(minu_ms, 60000).
-define(sec_ms, 1000).
-define(week_ms(N), (?week_ms * (N))).
-define(day_ms(N), (?day_ms * (N))).
-define(hour_ms(N), (?hour_ms * (N))).
-define(minu_ms(N), (?minu_ms * (N))).
-define(sec_ms(N), (?sec_ms * (N))).

-define(sync_dirty_interval, ?hour_ms).

%% 全局kv
-record(kv, {
    key,             %% 键
    value,           %% 值
    dirty = false    %% 是否更新
}).

-define(warn(Msg), lager:warning(Msg)).
-define(warn(F, Msg), lager:warning(io_lib:format(F, Msg)).
-define(info(Msg), lager:info(Msg)).
-define(info(F, Msg), lager:info(io_lib:format(F, Msg))).
-define(error(Msg), lager:error(Msg)).
-define(error(F, Msg), lager:error(io_lib:format(F, Msg))).