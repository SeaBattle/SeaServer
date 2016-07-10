%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2016 13:23
%%%-------------------------------------------------------------------
-module(ss_auth_logic).
-author("tihon").

-include_lib("seaconfig/include/sc_headers.hrl").

%% API
-export([auth_user/1, register_user/1]).

auth_user(Packet) ->
  ok.

register_user(Packet) ->
  Hosts = sc_conf_holder:get_conf(?USER_SERVICE_HOSTS),
  %TODO select server
  ok.

