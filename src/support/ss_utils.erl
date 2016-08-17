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
-export([pid_to_uid/1, uid_to_pid/1]).

-spec uid_to_pid(binary()) -> pid().  %TODO distributed pid
uid_to_pid(UID) when is_binary(UID) ->
  list_to_pid(binary_to_list(UID)).

-spec pid_to_uid(pid()) -> binary().  %TODO distributed pid
pid_to_uid(Pid) when is_pid(Pid) ->
  list_to_binary(pid_to_list(Pid)).