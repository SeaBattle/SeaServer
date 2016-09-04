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
-export([pid_to_uid/1, uid_to_pid/1, replace_at/3]).

-spec uid_to_pid(binary()) -> pid().  %TODO distributed pid
uid_to_pid(UID) when is_binary(UID) ->
  list_to_pid(binary_to_list(UID)).

-spec pid_to_uid(pid()) -> binary().  %TODO distributed pid
pid_to_uid(Pid) when is_pid(Pid) ->
  list_to_binary(pid_to_list(Pid)).

-spec replace_at(binary(), pos_integer(), binary()) -> binary().
replace_at(Binary, Start, Replacement)
  when is_binary(Binary), Start < byte_size(Binary), Start >= 0, Start + byte_size(Replacement) < byte_size(Binary) + 1 ->
  Before = binary:part(Binary, 0, Start),
  After = binary:part(Binary, Start + 1, byte_size(Binary) - Start - byte_size(Replacement)),
  <<Before/binary, Replacement/binary, After/binary>>.