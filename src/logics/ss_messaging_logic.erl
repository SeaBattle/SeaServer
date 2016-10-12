%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Oct 2016 16:15
%%%-------------------------------------------------------------------
-module(ss_messaging_logic).
-author("tihon").

-include("ss_headers.hrl").

%% API
-export([online_message/3]).

-spec online_message(ss_types:uid(), ss_types:uid(), map()) -> boolean().
online_message(To, From, Message) ->
  case syn:find_by_key(To) of
    Pid when is_pid(Pid) ->
      ss_handler_logic:process_package_to_client(Pid, Message#{?FROM_HEAD => From}),
      true;
    undefined ->
      false
  end.