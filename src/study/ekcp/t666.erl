%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% t666:test().
%%% @end
%%% Created : 17. 十一月 2021 20:02
%%%-------------------------------------------------------------------
-module(t666).
-author("liaoxifeng").

%% API
-export([test/0]).


test() ->
    Ref = ekcp:create(1, self()),
    ekcp:release(Ref),
    ekcp:flush(Ref).