%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Aug 2016 17:12
%%%-------------------------------------------------------------------
-module(ss_utils).
-author("tihon").

%% API
-export([replace_at/3]).

-spec replace_at(binary(), pos_integer(), binary()) -> binary().
replace_at(Binary, Start, Replacement)
  when is_binary(Binary), Start < byte_size(Binary), Start >= 0, Start + byte_size(Replacement) < byte_size(Binary) + 1 ->
  Before = binary:part(Binary, 0, Start),
  After = binary:part(Binary, Start + byte_size(Replacement), byte_size(Binary) - Start - byte_size(Replacement)),
  <<Before/binary, Replacement/binary, After/binary>>.