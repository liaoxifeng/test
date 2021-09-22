%%%-------------------------------------------------------------------
%%% @author liaoxifeng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% beam转源码
%%% @end
%%% Created : 22. 十月 2020 17:36
%%%-------------------------------------------------------------------
-module(beam).
-author("liaoxifeng").

%% API
-export([
    beam_to_source/1
    ,hdl/0
]).

%% 得到源代码 Beam.erl内容
beam_to_source(Beam) ->
   case beam_lib:chunks(code:which(Beam), [abstract_code]) of
       {ok,{_, [{abstract_code, {_, AC}}]}} ->
           Context = erl_prettypr:format(erl_syntax:form_list(AC)),
           create_file(Beam, Context);
       _ ->
           io:format("~w no_abstract_code", [Beam])
   end.

create_file(Beam, Context) ->
    File = "../out/" ++ to_list(Beam) ++ ".erl",
    {ok, Pid} = file:open(File, [write, raw]),
    file:write(Pid, Context),
    file:close(Pid).

hdl() ->
    {ok, List} = file:list_dir_all("../ebin"),
    F = fun(File) ->
        Beam = filename:rootname(File),
        beam_to_source(list_to_atom(Beam))
        end,
    lists:foreach(F, List).

to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Integer) when is_integer(Integer) -> integer_to_list(Integer);
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_list(L) -> L.
