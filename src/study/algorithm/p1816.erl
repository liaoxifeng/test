%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% $\ .   p1816:p().
%%% @end
%%% Created : 06. 十二月 2021 22:37
%%%-------------------------------------------------------------------
-module(p1816).
-author("liaoxifeng").

%% API
-export([p/0]).

p() ->
    S = <<"Hello how are you Contestant">>,
    K = 4,
    p(S, K, <<>>).

p(<<>>, _, Bit) -> Bit;
p(<<$\ :8, _/binary>>, 1, Bit) -> Bit;
p(<<$\ :8, B/binary>>, K, Bit) ->
    p(B, K - 1, <<Bit/binary, $\ :8>>);
p(<<H:8, B/binary>>, K, Bit) ->
    p(B, K, <<Bit/binary, H:8>>).